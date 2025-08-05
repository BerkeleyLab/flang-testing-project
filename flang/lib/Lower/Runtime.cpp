//===-- Runtime.cpp -------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "flang/Lower/Runtime.h"
#include "flang/Lower/Bridge.h"
#include "flang/Lower/Coarray.h"
#include "flang/Lower/OpenACC.h"
#include "flang/Lower/OpenMP.h"
#include "flang/Lower/StatementContext.h"
#include "flang/Optimizer/Builder/FIRBuilder.h"
#include "flang/Optimizer/Builder/Runtime/Coarray.h"
#include "flang/Optimizer/Builder/Runtime/RTBuilder.h"
#include "flang/Optimizer/Builder/Todo.h"
#include "flang/Optimizer/Dialect/FIROpsSupport.h"
#include "flang/Parser/parse-tree.h"
#include "flang/Runtime/misc-intrinsic.h"
#include "flang/Runtime/pointer.h"
#include "flang/Runtime/random.h"
#include "flang/Runtime/stop.h"
#include "flang/Runtime/time-intrinsic.h"
#include "flang/Semantics/tools.h"
#include "mlir/Dialect/OpenACC/OpenACC.h"
#include "mlir/Dialect/OpenMP/OpenMPDialect.h"
#include "llvm/Support/Debug.h"
#include <optional>

#define DEBUG_TYPE "flang-lower-runtime"

using namespace Fortran::runtime;

/// Runtime calls that do not return to the caller indicate this condition by
/// terminating the current basic block with an unreachable op.
static void genUnreachable(fir::FirOpBuilder &builder, mlir::Location loc) {
  mlir::Block *curBlock = builder.getBlock();
  mlir::Operation *parentOp = curBlock->getParentOp();
  if (parentOp->getDialect()->getNamespace() ==
      mlir::omp::OpenMPDialect::getDialectNamespace())
    Fortran::lower::genOpenMPTerminator(builder, parentOp, loc);
  else if (parentOp->getDialect()->getNamespace() ==
           mlir::acc::OpenACCDialect::getDialectNamespace())
    Fortran::lower::genOpenACCTerminator(builder, parentOp, loc);
  else
    builder.create<fir::UnreachableOp>(loc);
  mlir::Block *newBlock = curBlock->splitBlock(builder.getInsertionPoint());
  builder.setInsertionPointToStart(newBlock);
}

/// Test if an ExtendedValue is absent. This is used to test if an intrinsic
// argument are absent at compile time.
static bool isStaticallyAbsent(const fir::ExtendedValue &exv) {
  return !fir::getBase(exv);
}

/// Initializes values for STAT and ERRMSG
static std::pair<mlir::Value, mlir::Value> getStatAndErrmsg(
    Fortran::lower::AbstractConverter &converter, mlir::Location loc,
    const std::list<Fortran::parser::StatOrErrmsg> &statOrErrList) {
  fir::FirOpBuilder &builder = converter.getFirOpBuilder();
  Fortran::lower::StatementContext stmtCtx;
  fir::ExtendedValue errMsgExpr, statExpr;

  mlir::Value statAddr, errMsgAddr;
  for (const Fortran::parser::StatOrErrmsg &statOrErr : statOrErrList) {
    std::visit(Fortran::common::visitors{
                   [&](const Fortran::parser::StatVariable &statVar) {
                     statExpr = converter.genExprAddr(
                         loc, Fortran::semantics::GetExpr(statVar), stmtCtx);
                     statAddr = fir::getBase(statExpr);
                   },
                   [&](const Fortran::parser::MsgVariable &errMsgVar) {
                     fir::ExtendedValue errMsgExpr = converter.genExprAddr(
                         loc, Fortran::semantics::GetExpr(errMsgVar), stmtCtx);
                     errMsgAddr = builder.createBox(loc, errMsgExpr);
                   }},
               statOrErr.u);
  }

  if (isStaticallyAbsent(statExpr)) {
    statAddr = builder.create<fir::AbsentOp>(
        loc, builder.getRefType(builder.getI32Type()));
  }
  if (isStaticallyAbsent(errMsgExpr)) {
    errMsgAddr = builder.create<fir::AbsentOp>(
        loc, fir::BoxType::get(mlir::NoneType::get(builder.getContext())));
  }
  return {statAddr, errMsgAddr};
}

//===----------------------------------------------------------------------===//
// Misc. Fortran statements that lower to runtime calls
//===----------------------------------------------------------------------===//

void Fortran::lower::genStopStatement(
    Fortran::lower::AbstractConverter &converter,
    const Fortran::parser::StopStmt &stmt) {
  const bool isError = std::get<Fortran::parser::StopStmt::Kind>(stmt.t) ==
                       Fortran::parser::StopStmt::Kind::ErrorStop;
  fir::FirOpBuilder &builder = converter.getFirOpBuilder();
  mlir::Location loc = converter.getCurrentLocation();
  Fortran::lower::StatementContext stmtCtx;
  llvm::SmallVector<mlir::Value> operands;
  mlir::func::FuncOp callee;
  mlir::FunctionType calleeType;
  // First operand is stop code (zero if absent)
  mlir::Value stopCodeInt, stopCodeChar;
  if (const auto &code =
          std::get<std::optional<Fortran::parser::StopCode>>(stmt.t)) {
    auto expr =
        converter.genExprValue(*Fortran::semantics::GetExpr(*code), stmtCtx);
    LLVM_DEBUG(llvm::dbgs() << "stop expression: "; expr.dump();
               llvm::dbgs() << '\n');
    expr.match(
        [&](const fir::CharBoxValue &x) {
          callee = fir::runtime::getRuntimeFunc<mkRTKey(StopStatementText)>(
              loc, builder);
          calleeType = callee.getFunctionType();
          // Creates a pair of operands for the CHARACTER and its LEN.
          operands.push_back(
              builder.createConvert(loc, calleeType.getInput(0), x.getAddr()));
          operands.push_back(
              builder.createConvert(loc, calleeType.getInput(1), x.getLen()));
          stopCodeChar =
              fir::getBase(fir::factory::createBoxValue(builder, loc, x));
        },
        [&](fir::UnboxedValue x) {
          callee = fir::runtime::getRuntimeFunc<mkRTKey(StopStatement)>(
              loc, builder);
          calleeType = callee.getFunctionType();
          mlir::Value cast =
              builder.createConvert(loc, calleeType.getInput(0), x);
          operands.push_back(cast);
          stopCodeInt = x;
        },
        [&](auto) {
          mlir::emitError(loc, "unhandled expression in STOP");
          std::exit(1);
        });
  } else {
    callee = fir::runtime::getRuntimeFunc<mkRTKey(StopStatement)>(loc, builder);
    calleeType = callee.getFunctionType();
    // Default to values are advised in F'2023 11.4 p2.
    operands.push_back(builder.createIntegerConstant(
        loc, calleeType.getInput(0), isError ? 1 : 0));
  }

  // Second operand indicates ERROR STOP
  operands.push_back(builder.createIntegerConstant(
      loc, calleeType.getInput(operands.size()), isError));

  // Third operand indicates QUIET (default to false).
  mlir::Value q;
  if (const auto &quiet =
          std::get<std::optional<Fortran::parser::ScalarLogicalExpr>>(stmt.t)) {
    const SomeExpr *expr = Fortran::semantics::GetExpr(*quiet);
    assert(expr && "failed getting typed expression");
    q = fir::getBase(converter.genExprValue(*expr, stmtCtx));
    operands.push_back(
        builder.createConvert(loc, calleeType.getInput(operands.size()), q));
    q = fir::getBase(converter.genExprAddr(*expr, stmtCtx));
  } else {
    operands.push_back(builder.createIntegerConstant(
        loc, calleeType.getInput(operands.size()), 0));
  }

  if (converter.getLoweringOptions().getCoarrayFeature()) {
    fir::runtime::genStopCoarray(builder, loc, q, stopCodeInt, stopCodeChar,
                                 isError);
  }

  builder.create<fir::CallOp>(loc, callee, operands);
  auto blockIsUnterminated = [&builder]() {
    mlir::Block *currentBlock = builder.getBlock();
    return currentBlock->empty() ||
           !currentBlock->back().hasTrait<mlir::OpTrait::IsTerminator>();
  };
  if (blockIsUnterminated())
    genUnreachable(builder, loc);
}

void Fortran::lower::genFailImageStatement(
    Fortran::lower::AbstractConverter &converter) {
  fir::FirOpBuilder &builder = converter.getFirOpBuilder();
  mlir::Location loc = converter.getCurrentLocation();
  mlir::func::FuncOp callee =
      fir::runtime::getRuntimeFunc<mkRTKey(CloseAllExternalUnits)>(loc,
                                                                   builder);
  mlir::Value why = builder.create<fir::AbsentOp>(
      loc, fir::BoxType::get(mlir::NoneType::get(builder.getContext())));
  llvm::SmallVector<mlir::Value> args = {why};
  builder.create<fir::CallOp>(loc, callee, args);
  fir::runtime::genFailImageStatement(builder, loc);
  genUnreachable(builder, loc);
}

void Fortran::lower::genNotifyWaitStatement(
    Fortran::lower::AbstractConverter &converter,
    const Fortran::parser::NotifyWaitStmt &stmt) {
  mlir::Location loc = converter.getCurrentLocation();
  fir::FirOpBuilder &builder = converter.getFirOpBuilder();
  Fortran::lower::StatementContext stmtCtx;

  // Handle STAT ,ERRMSG and UNTIL_COUNT
  mlir::Value statAddr, errMsgAddr, untilCount;
  const auto &eventSpecList =
      std::get<std::list<Fortran::parser::EventWaitSpec>>(stmt.t);
  for (const Fortran::parser::EventWaitSpec &eventSpec : eventSpecList) {
    std::visit(
        Fortran::common::visitors{
            [&](const Fortran::parser::StatOrErrmsg &statOrErr) {
              std::visit(
                  Fortran::common::visitors{
                      [&](const Fortran::parser::StatVariable &statVar) {
                        statAddr = fir::getBase(converter.genExprAddr(
                            loc, Fortran::semantics::GetExpr(statVar),
                            stmtCtx));
                      },
                      [&](const Fortran::parser::MsgVariable &errMsgVar) {
                        errMsgAddr = fir::getBase(converter.genExprAddr(
                            loc, Fortran::semantics::GetExpr(errMsgVar),
                            stmtCtx));
                      },
                  },
                  statOrErr.u);
            },
            [&](const Fortran::parser::ScalarIntExpr &untilCountVar) {
              untilCount = fir::getBase(converter.genExprAddr(
                  loc, Fortran::semantics::GetExpr(untilCountVar), stmtCtx));
            },
        },
        eventSpec.u);
  }
  if (isStaticallyAbsent(untilCount))
    untilCount =
        builder.create<fir::AbsentOp>(loc, builder.getI64Type()).getResult();
  if (isStaticallyAbsent(statAddr))
    statAddr =
        builder.create<fir::AbsentOp>(loc, builder.getI32Type()).getResult();
  if (isStaticallyAbsent(errMsgAddr)) {
    errMsgAddr =
        builder
            .create<fir::AbsentOp>(loc, fir::BoxType::get(mlir::NoneType::get(
                                            builder.getContext())))
            .getResult();
  }

  // Handle NOTIFY-VAR
  mlir::Value notifyVarAddr = fir::getBase(converter.genExprAddr(
      loc,
      Fortran::semantics::GetExpr(
          std::get<Fortran::parser::Scalar<Fortran::parser::Variable>>(stmt.t)),
      stmtCtx));

  fir::runtime::genNotifyWaitStatement(builder, loc, notifyVarAddr, untilCount,
                                       statAddr, errMsgAddr);
}

void Fortran::lower::genEventPostStatement(
    Fortran::lower::AbstractConverter &converter,
    const Fortran::parser::EventPostStmt &stmt) {
  mlir::Location loc = converter.getCurrentLocation();
  Fortran::lower::checkCoarrayFeatureEnabled(converter, loc);
  fir::FirOpBuilder &builder = converter.getFirOpBuilder();
  Fortran::lower::StatementContext stmtCtx;

  // Handle STAT and ERRMSG values
  const auto &statOrErrList =
      std::get<std::list<Fortran::parser::StatOrErrmsg>>(stmt.t);
  auto [statAddr, errMsgAddr] = getStatAndErrmsg(converter, loc, statOrErrList);

  // Handle EVENT-VAR and IMAGE_NUMBER
  auto eventExpr = Fortran::semantics::GetExpr(
      std::get<Fortran::parser::EventVariable>(stmt.t));
  mlir::Value eventAddr =
      fir::getBase(converter.genExprAddr(loc, eventExpr, stmtCtx));
  mlir::Value handle = fir::runtime::getCoarrayHandle(builder, loc, eventAddr);
  mlir::Value image;
  if (auto coref{evaluate::ExtractCoarrayRef(eventExpr)}) {
    image = Fortran::lower::getImageIndexFromCosubscripts(
        converter, loc, coref.value(), eventAddr);
  } else {
    image = fir::runtime::getThisImage(builder, loc);
  }
  mlir::Value imageNum = builder.createTemporary(loc, builder.getI32Type());
  builder.create<fir::StoreOp>(loc, image, imageNum);

  mlir::Value offset =
      Fortran::lower::genByteOffset(converter, *eventExpr, loc);
  fir::runtime::genEventPostStatement(builder, loc, imageNum, handle, offset,
                                      statAddr, errMsgAddr);
}

void Fortran::lower::genEventWaitStatement(
    Fortran::lower::AbstractConverter &converter,
    const Fortran::parser::EventWaitStmt &stmt) {
  fir::FirOpBuilder &builder = converter.getFirOpBuilder();
  mlir::Location loc = converter.getCurrentLocation();
  Fortran::lower::checkCoarrayFeatureEnabled(converter, loc);
  Fortran::lower::StatementContext stmtCtx;

  // Handle STAT ,ERRMSG and UNTIL_COUNT
  mlir::Value statAddr, errMsgAddr, untilCount;
  const auto &eventSpecList =
      std::get<std::list<Fortran::parser::EventWaitSpec>>(stmt.t);
  for (const Fortran::parser::EventWaitSpec &eventSpec : eventSpecList) {
    std::visit(
        Fortran::common::visitors{
            [&](const Fortran::parser::StatOrErrmsg &statOrErr) {
              std::visit(
                  Fortran::common::visitors{
                      [&](const Fortran::parser::StatVariable &statVar) {
                        statAddr = fir::getBase(converter.genExprAddr(
                            loc, Fortran::semantics::GetExpr(statVar),
                            stmtCtx));
                      },
                      [&](const Fortran::parser::MsgVariable &errMsgVar) {
                        errMsgAddr = fir::getBase(converter.genExprAddr(
                            loc, Fortran::semantics::GetExpr(errMsgVar),
                            stmtCtx));
                      },
                  },
                  statOrErr.u);
            },
            [&](const Fortran::parser::ScalarIntExpr &untilCountVar) {
              untilCount = fir::getBase(converter.genExprAddr(
                  loc, Fortran::semantics::GetExpr(untilCountVar), stmtCtx));
            },
        },
        eventSpec.u);
  }
  if (isStaticallyAbsent(untilCount))
    untilCount =
        builder.create<fir::AbsentOp>(loc, builder.getI64Type()).getResult();
  if (isStaticallyAbsent(statAddr))
    statAddr =
        builder.create<fir::AbsentOp>(loc, builder.getI32Type()).getResult();
  if (isStaticallyAbsent(errMsgAddr)) {
    errMsgAddr =
        builder
            .create<fir::AbsentOp>(loc, fir::BoxType::get(mlir::NoneType::get(
                                            builder.getContext())))
            .getResult();
  }

  // Handle EVENT-VAR
  mlir::Value eventVarAddr = fir::getBase(converter.genExprAddr(
      loc,
      Fortran::semantics::GetExpr(
          std::get<Fortran::parser::EventVariable>(stmt.t)),
      stmtCtx));

  fir::runtime::genEventWaitStatement(builder, loc, eventVarAddr, untilCount,
                                      statAddr, errMsgAddr);
}

void Fortran::lower::genLockStatement(
    Fortran::lower::AbstractConverter &converter,
    const Fortran::parser::LockStmt &stmt) {
  fir::FirOpBuilder &builder = converter.getFirOpBuilder();
  mlir::Location loc = converter.getCurrentLocation();
  Fortran::lower::checkCoarrayFeatureEnabled(converter, loc);
  Fortran::lower::StatementContext stmtCtx;

  // Handle LOCK_VAR and REMOTE_IMAGE values
  mlir::Value remoteImage;
  auto lockExpr = Fortran::semantics::GetExpr(
      std::get<Fortran::parser::LockVariable>(stmt.t));
  mlir::Value lockVarAddr =
      fir::getBase(converter.genExprAddr(loc, lockExpr, stmtCtx));
  mlir::Value handle;
  if (auto coref{evaluate::ExtractCoarrayRef(lockExpr)}) {
    lockVarAddr = fir::getBase(converter.getSymbolExtendedValue(
        coref.value().GetLastSymbol(), nullptr));
    handle = fir::runtime::getCoarrayHandle(builder, loc, lockVarAddr);
    remoteImage = Fortran::lower::getImageIndexFromCosubscripts(
        converter, loc, coref.value(), handle);
  } else {
    remoteImage = fir::runtime::getThisImage(builder, loc);
    handle = fir::runtime::getCoarrayHandle(builder, loc, lockVarAddr);
  }
  mlir::Value refRemoteImage =
      builder.createTemporary(loc, builder.getI32Type());
  builder.create<fir::StoreOp>(loc, remoteImage, refRemoteImage);

  mlir::Value offset = Fortran::lower::genByteOffset(converter, *lockExpr, loc);

  // Handle ACQUIRED_LOCK, STAT and ERRMSG values
  fir::ExtendedValue acquiredLockExpr;
  mlir::Value acquiredLock;
  const auto &lockStatList =
      std::get<std::list<Fortran::parser::LockStmt::LockStat>>(stmt.t);
  std::list<Fortran::parser::StatOrErrmsg> statOrErrList;
  for (const Fortran::parser::LockStmt::LockStat &lockStat : lockStatList) {
    std::visit(
        Fortran::common::visitors{
            [&](const Fortran::parser::StatOrErrmsg &statOrErr) {
              // TODO: Handle STAT and ERRMSG
            },
            [&](const Fortran::parser::ScalarLogicalVariable &logicalVar) {
              acquiredLockExpr = converter.genExprAddr(
                  loc, Fortran::semantics::GetExpr(logicalVar), stmtCtx);
              acquiredLock = fir::getBase(acquiredLockExpr);
            },
        },
        lockStat.u);
  }
  auto [statAddr, errMsgAddr] = getStatAndErrmsg(converter, loc, statOrErrList);
  if (isStaticallyAbsent(acquiredLockExpr)) {
    acquiredLock = builder.create<fir::AbsentOp>(
        loc, fir::ReferenceType::get(builder.getI1Type()));
  }

  fir::runtime::genLockStatement(builder, loc, refRemoteImage, handle,
                                 acquiredLock, offset, statAddr, errMsgAddr);
}

void Fortran::lower::genUnlockStatement(
    Fortran::lower::AbstractConverter &converter,
    const Fortran::parser::UnlockStmt &stmt) {
  fir::FirOpBuilder &builder = converter.getFirOpBuilder();
  mlir::Location loc = converter.getCurrentLocation();
  Fortran::lower::checkCoarrayFeatureEnabled(converter, loc);
  Fortran::lower::StatementContext stmtCtx;

  // Handle STAT and ERRMSG values
  const auto &statOrErrList =
      std::get<std::list<Fortran::parser::StatOrErrmsg>>(stmt.t);
  auto [statAddr, errMsgAddr] = getStatAndErrmsg(converter, loc, statOrErrList);

  // Handle LOCK_VAR and REMOTE_IMAGE values
  mlir::Value remoteImage;
  auto lockExpr = Fortran::semantics::GetExpr(
      std::get<Fortran::parser::LockVariable>(stmt.t));
  mlir::Value lockVarAddr =
      fir::getBase(converter.genExprAddr(loc, lockExpr, stmtCtx));
  mlir::Value handle;
  if (auto coref{evaluate::ExtractCoarrayRef(lockExpr)}) {
    lockVarAddr = fir::getBase(converter.getSymbolExtendedValue(
        coref.value().GetLastSymbol(), nullptr));
    handle = fir::runtime::getCoarrayHandle(builder, loc, lockVarAddr);
    remoteImage = Fortran::lower::getImageIndexFromCosubscripts(
        converter, loc, coref.value(), handle);
  } else {
    handle = fir::runtime::getCoarrayHandle(builder, loc, lockVarAddr);
    remoteImage = fir::runtime::getThisImage(builder, loc);
  }
  mlir::Value refRemoteImage =
      builder.createTemporary(loc, builder.getI32Type());
  builder.create<fir::StoreOp>(loc, remoteImage, refRemoteImage);

  mlir::Value offset = Fortran::lower::genByteOffset(converter, *lockExpr, loc);
  fir::runtime::genUnlockStatement(builder, loc, refRemoteImage, handle, offset,
                                   statAddr, errMsgAddr);
}

void Fortran::lower::genSyncAllStatement(
    Fortran::lower::AbstractConverter &converter,
    const Fortran::parser::SyncAllStmt &stmt) {
  mlir::Location loc = converter.getCurrentLocation();
  Fortran::lower::checkCoarrayFeatureEnabled(converter, loc);

  // Handle STAT and ERRMSG values
  const std::list<Fortran::parser::StatOrErrmsg> &statOrErrList = stmt.v;
  auto [statAddr, errMsgAddr] = getStatAndErrmsg(converter, loc, statOrErrList);

  fir::FirOpBuilder &builder = converter.getFirOpBuilder();
  fir::runtime::genSyncAllStatement(builder, loc, statAddr, errMsgAddr);
}

void Fortran::lower::genSyncImagesStatement(
    Fortran::lower::AbstractConverter &converter,
    const Fortran::parser::SyncImagesStmt &stmt) {
  mlir::Location loc = converter.getCurrentLocation();
  fir::FirOpBuilder &builder = converter.getFirOpBuilder();

  // Handle STAT and ERRMSG values
  const std::list<Fortran::parser::StatOrErrmsg> &statOrErrList =
      std::get<std::list<Fortran::parser::StatOrErrmsg>>(stmt.t);
  auto [statAddr, errMsgAddr] = getStatAndErrmsg(converter, loc, statOrErrList);

  // SYNC_IMAGES(*) is passed as count == -1 while  SYNC IMAGES([]) hase count
  // == 0. Note further that SYNC IMAGES(*) is not semantically equivalent to
  // SYNC ALL.
  Fortran::lower::StatementContext stmtCtx;
  mlir::Value imageSet;
  const Fortran::parser::SyncImagesStmt::ImageSet &imgSet =
      std::get<Fortran::parser::SyncImagesStmt::ImageSet>(stmt.t);
  std::visit(
      Fortran::common::visitors{
          [&](const Fortran::parser::IntExpr &intExpr) {
            const SomeExpr *expr = Fortran::semantics::GetExpr(intExpr);
            imageSet = fir::getBase(converter.genExprBox(loc, *expr, stmtCtx));
          },
          [&](const Fortran::parser::Star &) {
            imageSet = builder.create<fir::AbsentOp>(
                loc,
                fir::BoxType::get(mlir::NoneType::get(builder.getContext())));
          }},
      imgSet.u);

  fir::runtime::genSyncImagesStatement(builder, loc, imageSet, statAddr,
                                       errMsgAddr);
}

void Fortran::lower::genSyncMemoryStatement(
    Fortran::lower::AbstractConverter &converter,
    const Fortran::parser::SyncMemoryStmt &stmt) {
  mlir::Location loc = converter.getCurrentLocation();
  Fortran::lower::checkCoarrayFeatureEnabled(converter, loc);

  // Handle STAT and ERRMSG values
  const std::list<Fortran::parser::StatOrErrmsg> &statOrErrList = stmt.v;
  auto [statAddr, errMsgAddr] = getStatAndErrmsg(converter, loc, statOrErrList);

  fir::FirOpBuilder &builder = converter.getFirOpBuilder();
  fir::runtime::genSyncMemoryStatement(builder, loc, statAddr, errMsgAddr);
}

void Fortran::lower::genSyncTeamStatement(
    Fortran::lower::AbstractConverter &converter,
    const Fortran::parser::SyncTeamStmt &stmt) {
  mlir::Location loc = converter.getCurrentLocation();

  // Handle TEAM
  Fortran::lower::StatementContext stmtCtx;
  const Fortran::parser::TeamValue &teamValue =
      std::get<Fortran::parser::TeamValue>(stmt.t);
  const SomeExpr *teamExpr = Fortran::semantics::GetExpr(teamValue);
  mlir::Value team =
      fir::getBase(converter.genExprBox(loc, *teamExpr, stmtCtx));

  // Handle STAT and ERRMSG values
  const std::list<Fortran::parser::StatOrErrmsg> &statOrErrList =
      std::get<std::list<Fortran::parser::StatOrErrmsg>>(stmt.t);
  auto [statAddr, errMsgAddr] = getStatAndErrmsg(converter, loc, statOrErrList);

  fir::FirOpBuilder &builder = converter.getFirOpBuilder();
  fir::runtime::genSyncTeamStatement(builder, loc, team, statAddr, errMsgAddr);
}

void Fortran::lower::genPauseStatement(
    Fortran::lower::AbstractConverter &converter,
    const Fortran::parser::PauseStmt &) {
  fir::FirOpBuilder &builder = converter.getFirOpBuilder();
  mlir::Location loc = converter.getCurrentLocation();
  mlir::func::FuncOp callee =
      fir::runtime::getRuntimeFunc<mkRTKey(PauseStatement)>(loc, builder);
  builder.create<fir::CallOp>(loc, callee, std::nullopt);
}

void Fortran::lower::genPointerAssociate(fir::FirOpBuilder &builder,
                                         mlir::Location loc,
                                         mlir::Value pointer,
                                         mlir::Value target) {
  mlir::func::FuncOp func =
      fir::runtime::getRuntimeFunc<mkRTKey(PointerAssociate)>(loc, builder);
  llvm::SmallVector<mlir::Value> args = fir::runtime::createArguments(
      builder, loc, func.getFunctionType(), pointer, target);
  builder.create<fir::CallOp>(loc, func, args);
}

void Fortran::lower::genPointerAssociateRemapping(fir::FirOpBuilder &builder,
                                                  mlir::Location loc,
                                                  mlir::Value pointer,
                                                  mlir::Value target,
                                                  mlir::Value bounds) {
  mlir::func::FuncOp func =
      fir::runtime::getRuntimeFunc<mkRTKey(PointerAssociateRemapping)>(loc,
                                                                       builder);
  auto fTy = func.getFunctionType();
  auto sourceFile = fir::factory::locationToFilename(builder, loc);
  auto sourceLine =
      fir::factory::locationToLineNo(builder, loc, fTy.getInput(4));
  llvm::SmallVector<mlir::Value> args = fir::runtime::createArguments(
      builder, loc, func.getFunctionType(), pointer, target, bounds, sourceFile,
      sourceLine);
  builder.create<fir::CallOp>(loc, func, args);
}

void Fortran::lower::genPointerAssociateLowerBounds(fir::FirOpBuilder &builder,
                                                    mlir::Location loc,
                                                    mlir::Value pointer,
                                                    mlir::Value target,
                                                    mlir::Value lbounds) {
  mlir::func::FuncOp func =
      fir::runtime::getRuntimeFunc<mkRTKey(PointerAssociateLowerBounds)>(
          loc, builder);
  llvm::SmallVector<mlir::Value> args = fir::runtime::createArguments(
      builder, loc, func.getFunctionType(), pointer, target, lbounds);
  builder.create<fir::CallOp>(loc, func, args);
}
