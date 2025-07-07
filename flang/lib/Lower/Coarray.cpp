//===-- Coarray.cpp -------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// Implementation of the lowering of image related constructs and expressions.
/// Fortran images can form teams, communicate via coarrays, etc.
///
//===----------------------------------------------------------------------===//

#include "flang/Lower/Coarray.h"
#include "flang/Evaluate/fold.h"
#include "flang/Lower/AbstractConverter.h"
#include "flang/Lower/Allocatable.h"
#include "flang/Lower/ConvertCall.h"
#include "flang/Lower/ConvertExprToHLFIR.h"
#include "flang/Lower/ConvertType.h"
#include "flang/Lower/PFTBuilder.h"
#include "flang/Lower/StatementContext.h"
#include "flang/Lower/Support/Utils.h"
#include "flang/Lower/SymbolMap.h"
#include "flang/Optimizer/Builder/FIRBuilder.h"
#include "flang/Optimizer/Builder/HLFIRTools.h"
#include "flang/Optimizer/Builder/MutableBox.h"
#include "flang/Optimizer/Builder/Runtime/Allocatable.h"
#include "flang/Optimizer/Builder/Runtime/Coarray.h"
#include "flang/Optimizer/Builder/Runtime/Derived.h"
#include "flang/Optimizer/Builder/Runtime/Inquiry.h"
#include "flang/Optimizer/Builder/Runtime/Pointer.h"
#include "flang/Optimizer/Builder/Runtime/RTBuilder.h"
#include "flang/Optimizer/Builder/Todo.h"
#include "flang/Optimizer/HLFIR/HLFIROps.h"
#include "flang/Optimizer/Support/DataLayout.h"
#include "flang/Optimizer/Support/InternalNames.h"
#include "flang/Optimizer/Support/Utils.h"
#include "flang/Parser/parse-tree.h"
#include "flang/Runtime/allocatable.h"
#include "flang/Semantics/expression.h"
#include "flang/Semantics/runtime-type-info.h"
#include "flang/Semantics/tools.h"
#include "mlir/Dialect/LLVMIR/LLVMDialect.h"

using namespace Fortran::semantics;
using namespace Fortran::runtime;

/// Test if an ExtendedValue is absent.
static bool isStaticallyAbsent(const fir::ExtendedValue &exv) {
  return !fir::getBase(exv);
}

//===----------------------------------------------------------------------===//
// TEAM statements and constructs
//===----------------------------------------------------------------------===//

void Fortran::lower::genChangeTeamConstruct(
    Fortran::lower::AbstractConverter &converter,
    Fortran::lower::pft::Evaluation &,
    const Fortran::parser::ChangeTeamConstruct &) {
  // TODO(converter.getCurrentLocation(), "coarray: CHANGE TEAM construct");
}

void Fortran::lower::genChangeTeamStmt(
    Fortran::lower::AbstractConverter &converter,
    Fortran::lower::pft::Evaluation &,
    const Fortran::parser::ChangeTeamStmt &stmt) {
  mlir::Location loc = converter.getCurrentLocation();
  Fortran::lower::checkCoarrayFeatureEnabled(converter, loc);
  fir::FirOpBuilder &builder = converter.getFirOpBuilder();

  mlir::Value errMsg, stat, team;
  // Handle STAT and ERRMSG values
  Fortran::lower::StatementContext stmtCtx;
  const std::list<Fortran::parser::StatOrErrmsg> &statOrErrList =
      std::get<std::list<Fortran::parser::StatOrErrmsg>>(stmt.t);
  for (const Fortran::parser::StatOrErrmsg &statOrErr : statOrErrList) {
    std::visit(
        Fortran::common::visitors{
            [&](const Fortran::parser::StatVariable &statVar) {
              const auto *expr = Fortran::semantics::GetExpr(statVar);
              stat = fir::getBase(converter.genExprAddr(loc, *expr, stmtCtx));
            },
            [&](const Fortran::parser::MsgVariable &errMsgVar) {
              const auto *expr = Fortran::semantics::GetExpr(errMsgVar);
              errMsg = fir::getBase(converter.genExprBox(loc, *expr, stmtCtx));
            },
        },
        statOrErr.u);
  }

  if (isStaticallyAbsent(stat))
    stat = builder.create<fir::AbsentOp>(
        loc, builder.getRefType(builder.getI32Type()));
  if (isStaticallyAbsent(errMsg))
    errMsg = builder.create<fir::AbsentOp>(
        loc, fir::BoxType::get(mlir::NoneType::get(builder.getContext())));

  // Handle TEAM-VALUE
  const auto *teamExpr =
      Fortran::semantics::GetExpr(std::get<Fortran::parser::TeamValue>(stmt.t));
  team = fir::getBase(converter.genExprBox(loc, *teamExpr, stmtCtx));

  fir::runtime::genChangeTeamStatement(builder, loc, team, stat, errMsg);
}

void Fortran::lower::genEndChangeTeamStmt(
    Fortran::lower::AbstractConverter &converter,
    Fortran::lower::pft::Evaluation &,
    const Fortran::parser::EndChangeTeamStmt &stmt) {
  mlir::Location loc = converter.getCurrentLocation();
  Fortran::lower::checkCoarrayFeatureEnabled(converter, loc);
  fir::FirOpBuilder &builder = converter.getFirOpBuilder();

  mlir::Value errMsg, stat;
  // Handle STAT and ERRMSG values
  Fortran::lower::StatementContext stmtCtx;
  const std::list<Fortran::parser::StatOrErrmsg> &statOrErrList =
      std::get<std::list<Fortran::parser::StatOrErrmsg>>(stmt.t);
  for (const Fortran::parser::StatOrErrmsg &statOrErr : statOrErrList) {
    std::visit(
        Fortran::common::visitors{
            [&](const Fortran::parser::StatVariable &statVar) {
              const auto *expr = Fortran::semantics::GetExpr(statVar);
              stat = fir::getBase(converter.genExprAddr(loc, *expr, stmtCtx));
            },
            [&](const Fortran::parser::MsgVariable &errMsgVar) {
              const auto *expr = Fortran::semantics::GetExpr(errMsgVar);
              errMsg = fir::getBase(converter.genExprBox(loc, *expr, stmtCtx));
            },
        },
        statOrErr.u);
  }

  if (isStaticallyAbsent(stat))
    stat = builder.create<fir::AbsentOp>(
        loc, builder.getRefType(builder.getI32Type()));
  if (isStaticallyAbsent(errMsg))
    errMsg = builder.create<fir::AbsentOp>(
        loc, fir::BoxType::get(mlir::NoneType::get(builder.getContext())));

  fir::runtime::genEndTeamStatement(builder, loc, stat, errMsg);
}

void Fortran::lower::genFormTeamStatement(
    Fortran::lower::AbstractConverter &converter,
    Fortran::lower::pft::Evaluation &,
    const Fortran::parser::FormTeamStmt &stmt) {
  mlir::Location loc = converter.getCurrentLocation();
  Fortran::lower::checkCoarrayFeatureEnabled(converter, loc);
  fir::FirOpBuilder &builder = converter.getFirOpBuilder();

  mlir::Value errMsg, stat, newIndex, teamNumber, team;
  // Handle NEW_INDEX, STAT and ERRMSG
  std::list<Fortran::parser::StatOrErrmsg> statOrErrList{};
  Fortran::lower::StatementContext stmtCtx;
  const auto &formSpecList =
      std::get<std::list<Fortran::parser::FormTeamStmt::FormTeamSpec>>(stmt.t);
  for (const Fortran::parser::FormTeamStmt::FormTeamSpec &formSpec :
       formSpecList) {
    std::visit(
        Fortran::common::visitors{
            [&](const Fortran::parser::StatOrErrmsg &statOrErr) {
              std::visit(
                  Fortran::common::visitors{
                      [&](const Fortran::parser::StatVariable &statVar) {
                        const auto *expr = Fortran::semantics::GetExpr(statVar);
                        stat = fir::getBase(
                            converter.genExprAddr(loc, *expr, stmtCtx));
                      },
                      [&](const Fortran::parser::MsgVariable &errMsgVar) {
                        const auto *expr =
                            Fortran::semantics::GetExpr(errMsgVar);
                        errMsg = fir::getBase(
                            converter.genExprBox(loc, *expr, stmtCtx));
                      },
                  },
                  statOrErr.u);
            },
            [&](const Fortran::parser::ScalarIntExpr &intExpr) {
              fir::ExtendedValue newIndexExpr = converter.genExprValue(
                  loc, Fortran::semantics::GetExpr(intExpr), stmtCtx);
              newIndex = fir::getBase(newIndexExpr);
            },
        },
        formSpec.u);
  }
  if (isStaticallyAbsent(newIndex))
    newIndex = builder.create<fir::AbsentOp>(
        loc, builder.getRefType(builder.getI32Type()));
  if (isStaticallyAbsent(stat))
    stat = builder.create<fir::AbsentOp>(
        loc, builder.getRefType(builder.getI32Type()));
  if (isStaticallyAbsent(errMsg))
    errMsg = builder.create<fir::AbsentOp>(
        loc, fir::BoxType::get(mlir::NoneType::get(builder.getContext())));

  // Handle TEAM-NUMBER
  const auto *teamNumberExpr = Fortran::semantics::GetExpr(
      std::get<Fortran::parser::ScalarIntExpr>(stmt.t));
  teamNumber = builder.createTemporary(loc, builder.getI64Type());
  builder.create<fir::StoreOp>(
      loc,
      builder.createConvert(
          loc, builder.getI64Type(),
          fir::getBase(converter.genExprValue(loc, *teamNumberExpr, stmtCtx))),
      teamNumber);

  // Handle TEAM-VARIABLE
  const auto *teamExpr = Fortran::semantics::GetExpr(
      std::get<Fortran::parser::TeamVariable>(stmt.t));
  team = fir::getBase(converter.genExprBox(loc, *teamExpr, stmtCtx));

  fir::runtime::genFormTeamStatement(builder, loc, teamNumber, team, newIndex,
                                     stat, errMsg);
}
//===----------------------------------------------------------------------===//
// CRITICAL construct
//===----------------------------------------------------------------------===//

mlir::Value
Fortran::lower::genCriticalCorarrayHandle(AbstractConverter &converter) {
  mlir::Location loc = converter.getCurrentLocation();
  fir::FirOpBuilder &builder = converter.getFirOpBuilder();

  // FIXME : The real size for critical type after PRIF spec updated
  fir::ExtendedValue emptyBox =
      builder.createTemporary(loc, builder.getI64Type());
  emptyBox = fir::factory::createBoxValue(builder, loc, emptyBox);
  mlir::Value sizeInBytes =
      Fortran::lower::getSizeInBytes(builder, loc, emptyBox);

  mlir::Type arrType = fir::SequenceType::get(
      {static_cast<fir::SequenceType::Extent>(1)}, builder.getI64Type());
  mlir::Type addrType = builder.getRefType(builder.getI64Type());
  mlir::Value index =
      builder.createIntegerConstant(loc, builder.getIndexType(), 1);
  mlir::Value lcobounds = builder.createTemporary(loc, arrType);
  mlir::Value lcoAddr =
      builder.create<fir::CoordinateOp>(loc, addrType, lcobounds, index);
  builder.create<fir::StoreOp>(
      loc, builder.createIntegerConstant(loc, builder.getI64Type(), 1),
      lcoAddr);
  lcobounds = builder.createBox(loc, lcobounds);

  mlir::Value ucobounds = builder.createTemporary(loc, arrType);
  mlir::Value ucoAddr =
      builder.create<fir::CoordinateOp>(loc, addrType, ucobounds, index);
  mlir::Value numImages = builder.createConvert(
      loc, builder.getI64Type(), fir::runtime::getNumImages(builder, loc));
  builder.create<fir::StoreOp>(loc, numImages, ucoAddr);
  ucobounds = builder.createBox(loc, ucobounds);

  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::Value allocMem = builder.createTemporary(loc, ptrTy);
  mlir::Value none = builder.create<fir::AbsentOp>(
      loc, fir::BoxType::get(mlir::NoneType::get(builder.getContext())));

  mlir::Value coarrayHandle = genAllocateCoarrayRuntimeCall(
      builder, loc, sizeInBytes, lcobounds, ucobounds, allocMem, none, none);
  mlir::Value criticalCoarray = builder.create<fir::AbsentOp>(
      loc, fir::BoxType::get(mlir::NoneType::get(builder.getContext())));
  return hlfir::genDeclare(loc, builder, criticalCoarray,
                           ".tmp.critical.coarray", {}, nullptr, {},
                           coarrayHandle)
      .getBase();
}

void Fortran::lower::genCriticalStmt(AbstractConverter &converter,
                                     pft::Evaluation &eval,
                                     const Fortran::parser::CriticalStmt &stmt,
                                     mlir::Value coarrayAddr) {
  mlir::Location loc = converter.getCurrentLocation();
  fir::FirOpBuilder &builder = converter.getFirOpBuilder();

  mlir::Value errMsg, stat;
  // Handle STAT and ERRMSG values
  Fortran::lower::StatementContext stmtCtx;
  const std::list<Fortran::parser::StatOrErrmsg> &statOrErrList =
      std::get<std::list<Fortran::parser::StatOrErrmsg>>(stmt.t);
  for (const Fortran::parser::StatOrErrmsg &statOrErr : statOrErrList) {
    std::visit(
        Fortran::common::visitors{
            [&](const Fortran::parser::StatVariable &statVar) {
              const auto *expr = Fortran::semantics::GetExpr(statVar);
              stat = fir::getBase(converter.genExprAddr(loc, *expr, stmtCtx));
            },
            [&](const Fortran::parser::MsgVariable &errMsgVar) {
              const auto *expr = Fortran::semantics::GetExpr(errMsgVar);
              errMsg = fir::getBase(converter.genExprBox(loc, *expr, stmtCtx));
            },
        },
        statOrErr.u);
  }

  if (isStaticallyAbsent(stat))
    stat = builder.create<fir::AbsentOp>(
        loc, builder.getRefType(builder.getI32Type()));
  if (isStaticallyAbsent(errMsg))
    errMsg = builder.create<fir::AbsentOp>(
        loc, fir::BoxType::get(mlir::NoneType::get(builder.getContext())));

  mlir::Value coarrayHandle =
      fir::runtime::getCoarrayHandle(builder, loc, coarrayAddr);
  fir::runtime::genCriticalStatement(builder, loc, coarrayHandle, stat, errMsg);
}

void Fortran::lower::genEndCriticalStmt(
    AbstractConverter &converter, pft::Evaluation &eval,
    const Fortran::parser::EndCriticalStmt &stmt, mlir::Value coarrayAddr) {
  mlir::Location loc = converter.getCurrentLocation();
  fir::FirOpBuilder &builder = converter.getFirOpBuilder();

  mlir::Value coarrayHandle =
      fir::runtime::getCoarrayHandle(builder, loc, coarrayAddr);
  fir::runtime::genEndCriticalStatement(builder, loc, coarrayHandle);
  mlir::Value none = builder.create<fir::AbsentOp>(
      loc, fir::BoxType::get(mlir::NoneType::get(builder.getContext())));
  Fortran::lower::genDeallocateCoarray(converter, loc, coarrayAddr, none);
}

//===----------------------------------------------------------------------===//
// COARRAY utils 
//===----------------------------------------------------------------------===//

llvm::SmallVector<mlir::Value, 4>
Fortran::lower::genCoshape(Fortran::lower::AbstractConverter &converter,
                           mlir::Location loc,
                           const Fortran::lower::SomeExpr &expr,
                           Fortran::lower::StatementContext &stmtCtx) {
  fir::FirOpBuilder &builder = converter.getFirOpBuilder();
  llvm::SmallVector<mlir::Value, 4> vecCoshape;

  auto *e = Fortran::evaluate::UnwrapExpr<Fortran::lower::SomeExpr>(expr);
  const Fortran::semantics::Symbol *sym = Fortran::evaluate::GetLastSymbol(*e);
  if (sym && sym->GetUltimate().has<Fortran::semantics::ObjectEntityDetails>()) {
    const auto *object{sym->GetUltimate().detailsIf<Fortran::semantics::ObjectEntityDetails>()};
    auto coshape = object->coshape();
    size_t corank = coshape.size();
    mlir::Type i64Ty = builder.getI64Type();

    for(size_t i = 0; i < corank; i++) {
      long int lcb = 1, ucb = -1; // default
      // Lower cobounds
      Fortran::semantics::Bound lcobound = coshape[i].lbound();
      if (lcobound.GetExplicit().has_value()) {
        auto b = Fortran::evaluate::ToInt64(lcobound.GetExplicit().value());
        if(b.has_value())
          lcb = b.value();
      }
      
      // Upper cobounds
      Fortran::semantics::Bound ucobound = coshape[i].ubound();
      if (ucobound.GetExplicit().has_value()) {
        auto b = Fortran::evaluate::ToInt64(ucobound.GetExplicit().value());
        if(b.has_value())
          ucb = b.value();
      }
      size_t coextent = ucb < lcb ? -1 : (ucb - lcb + 1);
      vecCoshape.push_back(builder.createIntegerConstant(loc, i64Ty, coextent));
    }
  }
  return vecCoshape;
}

llvm::SmallVector<mlir::Value, 4>
Fortran::lower::genCoSubscripts(Fortran::lower::AbstractConverter &converter,
                                mlir::Location loc,
                                const Fortran::lower::SomeExpr &expr,
                                Fortran::lower::StatementContext &stmtCtx) {
  llvm::SmallVector<mlir::Value, 4> imageIndex;

  if (auto coarrayRef{Fortran::evaluate::ExtractCoarrayRef(expr)}) {
    const Fortran::semantics::Symbol sym = coarrayRef.value().GetLastSymbol();
    if (sym.GetUltimate().has<Fortran::semantics::ObjectEntityDetails>()) {
      size_t corank = coarrayRef.value().cosubscript().size();
      for (unsigned i = 0; i < corank; i++) {
        auto c = ignoreEvConvert(coarrayRef.value().cosubscript()[i]);
        imageIndex.push_back(fir::getBase(converter.genExprValue(loc, c, stmtCtx)));
      }
    }
  }
  return imageIndex;
}

void Fortran::lower::checkCoarrayFeatureEnabled(AbstractConverter &converter,
                                                mlir::Location loc) {
  if (!converter.getLoweringOptions().getCoarrayFeature())
    mlir::emitError(loc, "Coarrays disabled, use '-fcoarray' to enable");
}

std::pair<mlir::Value, mlir::Value>
Fortran::lower::genCoarrayCoBounds(Fortran::lower::AbstractConverter &converter,
                                   mlir::Location loc,
                                   const Fortran::semantics::Symbol &sym) {
  fir::FirOpBuilder &builder = converter.getFirOpBuilder();
  mlir::Value ucobounds, lcobounds;

  if (sym.GetUltimate().has<Fortran::semantics::ObjectEntityDetails>()) {
    const auto *object{
        sym.GetUltimate().detailsIf<Fortran::semantics::ObjectEntityDetails>()};
    auto coshape = object->coshape();
    size_t corank = coshape.size();
    mlir::Type i64Ty = builder.getI64Type();
    mlir::Type addrType = builder.getRefType(i64Ty);
    mlir::Type arrayType = fir::SequenceType::get(
        {static_cast<fir::SequenceType::Extent>(corank)}, i64Ty);
    lcobounds = builder.createTemporary(loc, arrayType);
    ucobounds = builder.createTemporary(loc, arrayType);

    for (size_t i = 0; i < corank; i++) {
      long int lcobound = 1, ucobound = -1; // default cobound value
      auto index =
          builder.createIntegerConstant(loc, builder.getIndexType(), i);
      // Lower cobounds
      Fortran::semantics::Bound lbound = coshape[i].lbound();
      if (lbound.GetExplicit().has_value()) {
        auto b = Fortran::evaluate::ToInt64(lbound.GetExplicit().value());
        if (b.has_value())
          lcobound = b.value();
      }
      auto lcovalue = builder.createIntegerConstant(loc, i64Ty, lcobound);
      auto lcoaddr =
          builder.create<fir::CoordinateOp>(loc, addrType, lcobounds, index);
      builder.create<fir::StoreOp>(loc, lcovalue, lcoaddr);

      // Upper cobounds
      Fortran::semantics::Bound ubound = coshape[i].ubound();
      if (ubound.GetExplicit().has_value()) {
        auto b = Fortran::evaluate::ToInt64(ubound.GetExplicit().value());
        if (b.has_value())
          ucobound = b.value();
      }
      auto ucovalue = builder.createIntegerConstant(loc, i64Ty, ucobound);
      auto ucoaddr =
          builder.create<fir::CoordinateOp>(loc, addrType, ucobounds, index);
      builder.create<fir::StoreOp>(loc, ucovalue, ucoaddr);
    }
    lcobounds = builder.createBox(loc, lcobounds);
    ucobounds = builder.createBox(loc, ucobounds);
  }
  return {lcobounds, ucobounds};
}

/// From cosubscript, generate call to runtime function prif_image_index
/// associated to an addr
mlir::Value Fortran::lower::getImageIndexFromCosubscripts(
    Fortran::lower::AbstractConverter &converter, mlir::Location loc,
    const Fortran::evaluate::CoarrayRef &expr, mlir::Value handle) {
  fir::FirOpBuilder &builder = converter.getFirOpBuilder();
  Fortran::lower::StatementContext stmtCtx;

  // Creation of the cosubscripts array
  mlir::Type i64Ty = builder.getI64Type();
  unsigned corank = expr.cosubscript().size();
  mlir::Type indexType = builder.getIndexType();
  mlir::Type arrayType = fir::SequenceType::get(
      {static_cast<fir::SequenceType::Extent>(corank)}, i64Ty);
  mlir::Value cosubscripts = builder.createTemporary(loc, arrayType);
  mlir::Type addrType = builder.getRefType(i64Ty);
  for (unsigned dim = 0; dim < corank; ++dim) {
    auto image = ToInt64(expr.cosubscript()[dim]);
    mlir::Value idx;
    if (image.has_value())
      idx = builder.createIntegerConstant(loc, i64Ty, image.value());
    else {
      auto s = ignoreEvConvert(expr.cosubscript()[dim]);
      idx = builder.createConvert(
          loc, i64Ty, fir::getBase(converter.genExprValue(loc, s, stmtCtx)));
    }

    auto index = builder.createIntegerConstant(loc, indexType, dim);
    auto coAddr =
        builder.create<fir::CoordinateOp>(loc, addrType, cosubscripts, index);
    builder.create<fir::StoreOp>(loc, idx, coAddr);
  }
  cosubscripts = builder.createBox(loc, cosubscripts);

  // Computation of the image_index
  return fir::runtime::getImageIndex(builder, loc, handle, cosubscripts);
}

// Compute the size in bytes.
mlir::Value Fortran::lower::getSizeInBytes(fir::FirOpBuilder &builder,
                                           mlir::Location loc,
                                           fir::ExtendedValue box) {
  mlir::Type i64Ty = builder.getI64Type();
  mlir::Value boxVal = fir::getBase(box);
  mlir::Value sizeInBytes = builder.createTemporary(loc, i64Ty);
  mlir::Value size = builder.create<fir::BoxEleSizeOp>(loc, i64Ty, boxVal);
  if (fir::isArray(box)) {
    mlir::Value arraySize = builder.createConvert(
        loc, i64Ty, fir::runtime::genSize(builder, loc, boxVal));
    size = builder.create<mlir::arith::MulIOp>(loc, size, arraySize);
  }
  builder.create<fir::StoreOp>(loc, size, sizeInBytes);
  return sizeInBytes;
}

mlir::Value
Fortran::lower::genByteOffset(Fortran::lower::AbstractConverter &converter,
                              const Fortran::semantics::SomeExpr &expr,
                              mlir::Location loc) {
  fir::FirOpBuilder &builder = converter.getFirOpBuilder();
  std::optional<mlir::DataLayout> dl = fir::support::getOrSetMLIRDataLayout(
      builder.getModule(), /*allowDefaultLayout*/ true);
  Fortran::lower::StatementContext stmtCtx;

  mlir::Type i64Ty = builder.getI64Type();
  mlir::Value offset = builder.createTemporary(loc, i64Ty);
  if (auto dataRef{Fortran::evaluate::ExtractDataRef(expr)}) {
    fir::ExtendedValue exvBase =
        converter.getSymbolExtendedValue(dataRef->GetLastSymbol(), nullptr);
    hlfir::Entity base{Fortran::lower::extendedValueToHlfirEntity(
        loc, builder, exvBase, ".tmp.base_byteoffset")};
    size_t size =
        fir::getTypeSizeAndAlignment(loc, fir::getElementTypeOf(exvBase), *dl,
                                     builder.getKindMap())
            ->first;

    std::vector<Fortran::evaluate::Subscript> subscript;
    if (const auto *ref{
            std::get_if<Fortran::evaluate::ArrayRef>(&dataRef->u)}) {
      subscript = ref->subscript();
    } else if (auto coref{Fortran::evaluate::ExtractCoarrayRef(expr)}) {
      if (const auto *ref{
              std::get_if<Fortran::evaluate::ArrayRef>(&coref->base().u)})
        subscript = ref->subscript();
    }
    if (subscript.size() > 0) {
      mlir::Value elemLen = builder.createIntegerConstant(loc, i64Ty, size);
      mlir::Value strideInBytes = elemLen;
      mlir::Value offsetValue = builder.createIntegerConstant(loc, i64Ty, 0);
      mlir::Value one = builder.createIntegerConstant(loc, i64Ty, 1);
      for (auto ss : llvm::enumerate(subscript)) {
        size_t index = ss.index();
        mlir::Value lb, ub;
        mlir::Value lbBase = builder.createConvert(
            loc, i64Ty, hlfir::genLBound(loc, builder, base, index));
        mlir::Value extentBase = builder.createConvert(
            loc, i64Ty, hlfir::genExtent(loc, builder, base, index));
        Fortran::common::visit(
            Fortran::common::visitors{
                [&](const Fortran::evaluate::Triplet &trip) {
                  if (trip.lower().has_value()) {
                    auto lbExpr = ignoreEvConvert(trip.lower().value());
                    lb = builder.createConvert(
                        loc, i64Ty,
                        fir::getBase(converter.genExprValue(lbExpr, stmtCtx)));
                  } else {
                    lb = lbBase;
                  }
                },
                [&](const Fortran::evaluate::IndirectSubscriptIntegerExpr
                        &expr) {
                  if (expr.value().Rank() == 0) {
                    auto lbExpr = ignoreEvConvert(expr.value());
                    lb = builder.createConvert(
                        loc, i64Ty,
                        fir::getBase(converter.genExprValue(lbExpr, stmtCtx)));
                  } else {
                    auto vector = converter.genExprAddr(
                        ignoreEvConvert(expr.value()), stmtCtx);
                    lb = builder.createConvert(
                        loc, i64Ty,
                        fir::factory::readLowerBound(builder, loc, vector,
                                                     index, one));
                  }
                }},
            ss.value().u);
        lb = builder.create<mlir::arith::SubIOp>(loc, lb, lbBase);
        lb = builder.create<mlir::arith::MulIOp>(loc, lb, strideInBytes);
        offsetValue = builder.create<mlir::arith::AddIOp>(loc, offsetValue, lb);
        strideInBytes =
            builder.create<mlir::arith::MulIOp>(loc, strideInBytes, extentBase);
      }
      builder.create<fir::StoreOp>(loc, offsetValue, offset);
      return offset;
    }
  }
  builder.create<fir::StoreOp>(
      loc, builder.createIntegerConstant(loc, i64Ty, 0), offset);
  return offset;
}

//===----------------------------------------------------------------------===//
// COARRAY memory management
//===----------------------------------------------------------------------===//

mlir::Type Fortran::lower::getCoarrayHandleType(fir::FirOpBuilder &builder,
                                                mlir::Location loc) {
  // Defining the coarray handle type
  std::string handleDTName = PRIFTYPE("prif_coarray_handle");
  fir::RecordType handleTy =
      fir::RecordType::get(builder.getContext(), handleDTName);
  mlir::Type infoTy =
      fir::BoxType::get(fir::PointerType::get(builder.getNoneType()));
  handleTy.finalize({}, {{"info", infoTy}});

  // Checking if the type information was generated
  fir::TypeInfoOp dt;
  fir::RecordType parentType{};
  mlir::OpBuilder::InsertPoint insertPointIfCreated;
  std::tie(dt, insertPointIfCreated) =
      builder.createTypeInfoOp(loc, handleTy, parentType);
  if (insertPointIfCreated.isSet()) {
    // fir.type_info wasn't built in a previous call.
    dt->setAttr(dt.getNoInitAttrName(), builder.getUnitAttr());
    dt->setAttr(dt.getNoDestroyAttrName(), builder.getUnitAttr());
    dt->setAttr(dt.getNoFinalAttrName(), builder.getUnitAttr());
    builder.restoreInsertionPoint(insertPointIfCreated);
    // Create global op
    // FIXME: replace handleTy by the Derived type that describe handleTy
    std::string globalName =
        fir::NameUniquer::getTypeDescriptorName(handleDTName);
    auto linkage = builder.createLinkOnceODRLinkage();
    builder.createGlobal(loc, handleTy, globalName, linkage);
  }
  return handleTy;
}

mlir::Value Fortran::lower::genAllocateCoarrayRuntimeCall(
    fir::FirOpBuilder &builder, mlir::Location loc, mlir::Value sizeInBytes,
    mlir::Value lcobounds, mlir::Value ucobounds, mlir::Value allocMem,
    mlir::Value stat, mlir::Value errMsg) {
  // Generate call to prif_allocate_coarray with the correct mangling name
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy, ptrTy, ptrTy,
                                           ptrTy, ptrTy, ptrTy, ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("allocate_coarray"), ftype);
  fir::runtime::computeLastUcobound(builder, loc, lcobounds, ucobounds);

  // Allocate instance of prif_coarray_handle type based on the PRIF
  // specification.
  mlir::Type handleTy = Fortran::lower::getCoarrayHandleType(builder, loc);
  mlir::Value coarrayHandle =
      builder.createBox(loc, builder.createTemporary(loc, handleTy));

  // TODO: Handle FINAL_FUNC argument ("coarray_cleanup")
  mlir::Value finalFunc = builder.createTemporary(loc, ptrTy);
  mlir::Value none = builder.create<fir::AbsentOp>(
      loc, fir::BoxType::get(mlir::NoneType::get(builder.getContext())));

  llvm::SmallVector<mlir::Value> args = {lcobounds, ucobounds,     sizeInBytes,
                                         finalFunc, coarrayHandle, allocMem,
                                         stat,      none,          errMsg};
  builder.create<fir::CallOp>(loc, funcOp, args);
  return coarrayHandle;
}

/// Generate call to prif_allocate_coarray runtime subroutine based on
/// fir::MutableBoxValue and return a stat value.
mlir::Value Fortran::lower::genAllocateCoarray(
    Fortran::lower::AbstractConverter &converter, mlir::Location loc,
    const Fortran::semantics::Symbol &sym, fir::MutableBoxValue box,
    mlir::Value errMsgAddr) {
  Fortran::lower::checkCoarrayFeatureEnabled(converter, loc);
  fir::FirOpBuilder &builder = converter.getFirOpBuilder();

  // Handle LCOBOUNDS and UCOBOUNDS form the Fortran::semantics::Symbol
  auto [lcobounds, ucobounds] =
      Fortran::lower::genCoarrayCoBounds(converter, loc, sym);

  // Allocate reference for allocated_memory
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::Value allocMem = builder.createTemporary(loc, ptrTy);

  mlir::Value statAddr = builder.createTemporary(loc, builder.getI32Type());
  mlir::Value sizeInBytes = getSizeInBytes(
      builder, loc, fir::factory::createBoxValue(builder, loc, box));
  mlir::Value coarrayHandle =
      genAllocateCoarrayRuntimeCall(builder, loc, sizeInBytes, lcobounds,
                                    ucobounds, allocMem, statAddr, errMsgAddr);

  allocMem = builder.create<fir::LoadOp>(loc, allocMem);
  // Update the base_addr to the value. The allocation of the coarray is
  // performed during prif_allocate_coarray and we want an operation that
  // updates just the base_addr of the give box.
  fir::runtime::genPointerAssociateScalar(builder, loc, fir::getBase(box),
                                          allocMem);

  // Storing coarrayHandle to the variable declaration
  if (auto declare =
          mlir::dyn_cast<hlfir::DeclareOp>(box.getAddr().getDefiningOp())) {
    coarrayHandle = builder.createBox(loc, coarrayHandle);
    builder.create<fir::StoreOp>(loc, coarrayHandle,
                                 declare.getCoarrayHandle());
  }
  return builder.create<fir::LoadOp>(loc, statAddr);
}

/// Generate call to prif_allocate_coarray runtime subroutine
std::pair<mlir::Value, mlir::Value> Fortran::lower::genAllocateCoarray(
    Fortran::lower::AbstractConverter &converter, mlir::Location loc,
    const Fortran::semantics::Symbol &sym, fir::ExtendedValue preAlloc) {
  Fortran::lower::checkCoarrayFeatureEnabled(converter, loc);
  fir::FirOpBuilder &builder = converter.getFirOpBuilder();

  // Handle LCOBOUNDS and UCOBOUNDS form the Fortran::semantics::Symbol
  auto [lcobounds, ucobounds] =
      Fortran::lower::genCoarrayCoBounds(converter, loc, sym);

  // Allocate reference for allocated_memory
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::Value allocMem = builder.createTemporary(loc, ptrTy);

  // Handle SIZE_IN_BYTES
  mlir::Value sizeInBytes = getSizeInBytes(
      builder, loc, fir::factory::createBoxValue(builder, loc, preAlloc));

  // Handle STAT and ERRMSG
  mlir::Value stat = builder.create<fir::AbsentOp>(
      loc, builder.getRefType(builder.getI32Type()));
  mlir::Value errMsg = builder.create<fir::AbsentOp>(
      loc, fir::BoxType::get(mlir::NoneType::get(builder.getContext())));

  mlir::Value coarrayHandle = genAllocateCoarrayRuntimeCall(
      builder, loc, sizeInBytes, lcobounds, ucobounds, allocMem, stat, errMsg);

  allocMem =
      builder
          .create<fir::LoadOp>(loc, fir::getBase(preAlloc).getType(), allocMem)
          .getResult();
  return {allocMem, builder.createBox(loc, coarrayHandle)};
}

mlir::Value Fortran::lower::genDeallocateCoarray(
    Fortran::lower::AbstractConverter &converter, mlir::Location loc,
    fir::MutableBoxValue box, mlir::Value errMsgAddr) {
  fir::FirOpBuilder &builder = converter.getFirOpBuilder();
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::Value none = builder.create<fir::AbsentOp>(loc, ptrTy);
  mlir::Value stat = Fortran::lower::genDeallocateCoarray(
      converter, loc, box.getAddr(), errMsgAddr);
  fir::runtime::genPointerAssociateScalar(builder, loc, fir::getBase(box),
                                          none);
  return stat;
}

mlir::Value Fortran::lower::genDeallocateCoarray(
    Fortran::lower::AbstractConverter &converter, mlir::Location loc,
    mlir::Value coarrayAddr, mlir::Value errMsgAddr) {
  fir::FirOpBuilder &builder = converter.getFirOpBuilder();
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy, ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("deallocate_coarray"), ftype);

  // PRIF define prif_deallocate_coarray where the coarray_handles arg is an
  // array of handles, but this function if called by genDeallocate in
  // flang/lib/Lower/Allocatable.cpp and this function treat only one entity
  mlir::Value coarrayHandle =
      fir::runtime::getCoarrayHandle(builder, loc, coarrayAddr);
  mlir::Value stat = builder.createTemporary(loc, builder.getI32Type());
  builder.create<fir::StoreOp>(
      loc, builder.createIntegerConstant(loc, builder.getI32Type(), 0), stat);
  auto nullPtr = builder.createNullConstant(loc, ptrTy);
  llvm::SmallVector<mlir::Value> localArgs = {coarrayHandle, stat, nullPtr,
                                              errMsgAddr};
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
  return builder.create<fir::LoadOp>(loc, stat);
}

//===----------------------------------------------------------------------===//
// COARRAY expressions
//===----------------------------------------------------------------------===//

fir::ExtendedValue Fortran::lower::CoarrayExprHelper::genAddr(
    const Fortran::evaluate::CoarrayRef &expr) {
  TODO(converter.getCurrentLocation(), "co-array address");
}

fir::ExtendedValue Fortran::lower::CoarrayExprHelper::genValue(
    const Fortran::evaluate::CoarrayRef &expr) {
  TODO(converter.getCurrentLocation(), "co-array value");
}
