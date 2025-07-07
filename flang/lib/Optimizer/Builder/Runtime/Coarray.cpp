//===-- Coarray.cpp -- runtime API for coarray intrinsics -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "flang/Lower/Coarray.h"
#include "flang/Optimizer/Builder/Character.h"
#include "flang/Optimizer/Builder/FIRBuilder.h"
#include "flang/Optimizer/Builder/MutableBox.h"
#include "flang/Optimizer/Builder/Runtime/Allocatable.h"
#include "flang/Optimizer/Builder/Runtime/Coarray.h"
#include "flang/Optimizer/Builder/Runtime/Derived.h"
#include "flang/Optimizer/Builder/Runtime/RTBuilder.h"
#include "flang/Optimizer/Builder/Todo.h"
#include "flang/Optimizer/HLFIR/HLFIROps.h"
#include "flang/Optimizer/Support/InternalNames.h"
#include "flang/Runtime/coarray.h"
#include "flang/Semantics/scope.h"
#include "flang/Semantics/tools.h"
#include "mlir/Dialect/Func/IR/FuncOps.h"
#include "mlir/Dialect/LLVMIR/LLVMDialect.h"
#include "llvm/ADT/TypeSwitch.h"

using namespace Fortran::runtime;
using namespace Fortran::semantics;

/// Test if an ExtendedValue is absent.
static bool isStaticallyAbsent(const fir::ExtendedValue &exv) {
  return !fir::getBase(exv);
}

/// Generate call to runtime function to retrieve prif_coarray_handle
/// associated to an addr
mlir::Value fir::runtime::getCoarrayHandle(fir::FirOpBuilder &builder,
                                           mlir::Location loc,
                                           mlir::Value addr) {
  while (true) {
    mlir::Operation *defOp = addr.getDefiningOp();
    if (auto op = mlir::dyn_cast<fir::LoadOp>(defOp)) {
      addr = op.getMemref();
    } else if (auto op = mlir::dyn_cast<fir::BoxAddrOp>(defOp)) {
      addr = op.getVal();
    } else if (auto op = mlir::dyn_cast<fir::EmboxOp>(defOp)) {
      addr = op.getMemref();
    } else if (auto op = mlir::dyn_cast<fir::EmboxCharOp>(defOp)) {
      addr = op.getMemref();
    } else if (auto op = mlir::dyn_cast<hlfir::DesignateOp>(defOp)) {
      addr = op.getMemref();
    } else if (auto op = mlir::dyn_cast<fir::ConvertOp>(defOp)) {
      addr = op.getValue();
    } else {
      break;
    }
  }

  if (auto declare = mlir::dyn_cast<hlfir::DeclareOp>(addr.getDefiningOp())) {
    mlir::Value coarrayHandle = declare.getCoarrayHandle();
    if (isStaticallyAbsent(coarrayHandle))
      fir::emitFatalError(loc, "Unable to find the coarray_handle.", false);
    if (mlir::isa<fir::ReferenceType>(coarrayHandle.getType()))
      return builder.create<fir::LoadOp>(loc, coarrayHandle);
    return coarrayHandle;
  }
  addr.dump();
  TODO(loc, "Retrieve the coarray handle from this operation.");
}

/// Generate call to runtime function to compute the lastest ucobound.
void fir::runtime::computeLastUcobound(fir::FirOpBuilder &builder,
                                       mlir::Location loc,
                                       mlir::Value lcobounds,
                                       mlir::Value ucobounds) {
  mlir::func::FuncOp func =
      fir::runtime::getRuntimeFunc<mkRTKey(ComputeLastUcobound)>(loc, builder);
  mlir::Value num_images = fir::runtime::getNumImages(builder, loc);
  llvm::SmallVector<mlir::Value> args = {num_images, lcobounds, ucobounds};
  builder.create<fir::CallOp>(loc, func, args);
}

void fir::runtime::copy1DArrayToI64Array(fir::FirOpBuilder &builder,
                                         mlir::Location loc, mlir::Value from,
                                         mlir::Value to) {
  mlir::func::FuncOp func =
      fir::runtime::getRuntimeFunc<mkRTKey(Copy1DArrayToI64Array)>(loc,
                                                                   builder);
  llvm::SmallVector<mlir::Value> args = {from, to};
  builder.create<fir::CallOp>(loc, func, args);
}

/// Generate Call to runtime prif_init
mlir::Value fir::runtime::genInitCoarray(fir::FirOpBuilder &builder,
                                         mlir::Location loc) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::Value result = builder.create<fir::AllocaOp>(loc, builder.getI32Type());
  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("init"), ftype);
  llvm::SmallVector<mlir::Value> args =
      fir::runtime::createArguments(builder, loc, ftype, result);
  builder.create<fir::CallOp>(loc, funcOp, args);
  return builder.create<fir::LoadOp>(loc, result);
}

/// Generate Call to runtime prif_stop or error_stop
void fir::runtime::genStopCoarray(fir::FirOpBuilder &builder,
                                  mlir::Location loc, mlir::Value quiet,
                                  mlir::Value stopCodeInt,
                                  mlir::Value stopCodeChar, bool isError) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::Type boxTy = fir::BoxType::get(builder.getNoneType());
  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy, boxTy);
  mlir::func::FuncOp funcOp = builder.createFunction(
      loc, isError ? PRIFNAME_SUB("error_stop") : PRIFNAME_SUB("stop"), ftype);

  if (isStaticallyAbsent(quiet)) {
    quiet = builder.createTemporary(loc, builder.getI1Type());
    mlir::Value t =
        builder.createIntegerConstant(loc, builder.getI1Type(), true);
    builder.create<fir::StoreOp>(loc, t, quiet);
  }
  if (isStaticallyAbsent(stopCodeInt)) {
    stopCodeInt = builder.create<fir::AbsentOp>(
        loc, fir::BoxType::get(builder.getNoneType()));
  }
  if (isStaticallyAbsent(stopCodeChar)) {
    stopCodeChar = builder.create<fir::AbsentOp>(
        loc, fir::BoxType::get(builder.getNoneType()));
  }
  llvm::SmallVector<mlir::Value> args = fir::runtime::createArguments(
      builder, loc, ftype, quiet, stopCodeInt, stopCodeChar);
  builder.create<fir::CallOp>(loc, funcOp, args);
}

/// Generate Call to runtime prif_num_images
mlir::Value fir::runtime::getNumImages(fir::FirOpBuilder &builder,
                                       mlir::Location loc) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());

  mlir::Value result = builder.create<fir::AllocaOp>(loc, builder.getI32Type());
  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("num_images"), ftype);
  llvm::SmallVector<mlir::Value> args = {result};
  builder.create<fir::CallOp>(loc, funcOp, args);
  return builder.create<fir::LoadOp>(loc, result);
}

mlir::Value fir::runtime::getNumImagesWithTeam(fir::FirOpBuilder &builder,
                                               mlir::Location loc,
                                               mlir::Value team) {
  std::string numImagesName =
      fir::unwrapPassByRefType(team.getType()).isInteger()
          ? PRIFNAME_SUB("num_images_with_team_number")
          : PRIFNAME_SUB("num_images_with_team");

  mlir::Value result = builder.create<fir::AllocaOp>(loc, builder.getI32Type());
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy);
  mlir::func::FuncOp funcOp = builder.createFunction(loc, numImagesName, ftype);
  llvm::SmallVector<mlir::Value> args = {team, result};
  builder.create<fir::CallOp>(loc, funcOp, args);
  return builder.create<fir::LoadOp>(loc, result);
}

/// Generate Call to runtime prif_this_image_no_coarray
mlir::Value fir::runtime::getThisImage(fir::FirOpBuilder &builder,
                                       mlir::Location loc, mlir::Value team) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("this_image_no_coarray"), ftype);

  mlir::Value result = builder.create<fir::AllocaOp>(loc, builder.getI32Type());
  mlir::Value teamArg =
      !isStaticallyAbsent(team)
          ? team
          : builder.create<fir::AbsentOp>(
                loc,
                fir::BoxType::get(mlir::NoneType::get(builder.getContext())));
  llvm::SmallVector<mlir::Value> args = {teamArg, result};
  builder.create<fir::CallOp>(loc, funcOp, args);
  return builder.create<fir::LoadOp>(loc, result);
}

/// Generate Call to runtime prif_this_image_with_coarray or
/// prif_this_image_with_dim
mlir::Value fir::runtime::getThisImageWithCoarray(
    fir::FirOpBuilder &builder, mlir::Location loc, mlir::Type resultType,
    mlir::Value coarrayHandle, mlir::Value team, mlir::Value dim) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  llvm::SmallVector<mlir::Value> args;
  mlir::FunctionType ftype;
  mlir::func::FuncOp funcOp;
  mlir::Value result;
  if (!isStaticallyAbsent(dim)) {
    result = builder.create<fir::AllocaOp>(loc, resultType);
    ftype = PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy, ptrTy);
    funcOp =
        builder.createFunction(loc, PRIFNAME_SUB("this_image_with_dim"), ftype);
    args.insert(args.end(), {coarrayHandle, dim});
  } else {
    // Need to embox the array
    result = builder.createBox(loc, builder.createTemporary(loc, resultType));
    ftype = PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy);
    funcOp = builder.createFunction(
        loc, PRIFNAME_SUB("this_image_with_coarray"), ftype);
    args.push_back(coarrayHandle);
  }

  args.insert(args.end(), {team, result});
  builder.create<fir::CallOp>(loc, funcOp, args);
  return !isStaticallyAbsent(dim) ? builder.create<fir::LoadOp>(loc, result)
                                  : result;
}

/// Generate Call to runtime prif_image_status
mlir::Value fir::runtime::getImageStatus(fir::FirOpBuilder &builder,
                                         mlir::Location loc, mlir::Value image,
                                         mlir::Value team) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::Value result = builder.createTemporary(loc, builder.getI32Type());

  if (isStaticallyAbsent(team)) {
    team = builder.create<fir::AbsentOp>(
        loc, fir::BoxType::get(mlir::NoneType::get(builder.getContext())));
  }
  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("image_status"), ftype);
  llvm::SmallVector<mlir::Value> localArgs = {image, team, result};
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
  return builder.create<fir::LoadOp>(loc, result);
}

/// Generate call to runtime prif_this_image_index and assumed that sub is
/// an array of i64 elements
mlir::Value fir::runtime::getImageIndex(fir::FirOpBuilder &builder,
                                        mlir::Location loc, mlir::Value handle,
                                        mlir::Value sub, mlir::Value team) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::Value result = builder.create<fir::AllocaOp>(loc, builder.getI32Type());

  mlir::func::FuncOp funcOp;
  llvm::SmallVector<mlir::Value> localArgs = {handle, sub};
  if (isStaticallyAbsent(team)) {
    mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy);
    funcOp = builder.createFunction(loc, PRIFNAME_SUB("image_index"), ftype);
    localArgs.emplace_back(result);
  } else {
    std::string imageIndexName =
        fir::unwrapPassByRefType(team.getType()).isInteger()
            ? PRIFNAME_SUB("image_index_with_team")
            : PRIFNAME_SUB("image_index_with_team_number");
    mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy, ptrTy);
    funcOp = builder.createFunction(loc, imageIndexName, ftype);
    localArgs.insert(localArgs.end(), {team, result});
  }
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
  return builder.create<fir::LoadOp>(loc, result);
}

mlir::Value fir::runtime::getImageIndexFromBox(fir::FirOpBuilder &builder,
                                               mlir::Location loc,
                                               fir::ExtendedValue b,
                                               mlir::Value handle) {

  if (const auto *box = b.getBoxOf<fir::BoxValue>()) {
    const auto boxCosubs = box->getCosubscripts();
    if (!boxCosubs.size())
      return fir::runtime::getThisImage(builder, loc);
    // Creation of the cosubscripts array
    mlir::Type i64Ty = builder.getI64Type();
    mlir::Type arrayType = fir::SequenceType::get(
        {static_cast<fir::SequenceType::Extent>(box->corank())}, i64Ty);
    mlir::Value cosubscripts = builder.createTemporary(loc, arrayType);

    mlir::Type indexType = builder.getIndexType();
    mlir::Type addrType = builder.getRefType(i64Ty);
    for (unsigned dim = 0; dim < box->corank(); ++dim) {
      auto index = builder.createIntegerConstant(loc, indexType, dim);
      auto addr =
          builder.create<fir::CoordinateOp>(loc, addrType, cosubscripts, index);
      builder.create<fir::StoreOp>(loc, boxCosubs[dim], addr);
    }
    cosubscripts = builder.createBox(loc, cosubscripts);

    if (isStaticallyAbsent(handle)) {
      mlir::Value coarrayAddr = builder.create<fir::BoxAddrOp>(
          loc, box->getMemTy(), fir::getBase(*box));
      handle = fir::runtime::getCoarrayHandle(builder, loc, coarrayAddr);
    }
    // Computation of the image_index
    return fir::runtime::getImageIndex(builder, loc, handle, cosubscripts);
  }
  return {};
}

/// Generate Call to runtime prif_lcobound_{with|no}_dim
fir::ExtendedValue fir::runtime::genLCoBounds(fir::FirOpBuilder &builder,
                                              mlir::Location loc,
                                              mlir::Value handle, size_t corank,
                                              mlir::Value dim) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());

  mlir::func::FuncOp funcOp;
  llvm::SmallVector<mlir::Value> localArgs = {handle};
  if (isStaticallyAbsent(dim)) {
    llvm::SmallVector<mlir::Value, 1> extents{
        builder.createIntegerConstant(loc, builder.getIndexType(), corank)};
    mlir::Type resultType = fir::SequenceType::get(
        static_cast<fir::SequenceType::Extent>(corank), builder.getI64Type());
    mlir::Value result =
        builder.createBox(loc, builder.createTemporary(loc, resultType));
    mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy);
    funcOp =
        builder.createFunction(loc, PRIFNAME_SUB("lcobound_no_dim"), ftype);
    localArgs.emplace_back(result);
    builder.create<fir::CallOp>(loc, funcOp, localArgs);
    return fir::ArrayBoxValue(result, extents);
  } else {
    mlir::Value result = builder.createTemporary(loc, builder.getI64Type());
    mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy);
    funcOp =
        builder.createFunction(loc, PRIFNAME_SUB("lcobound_with_dim"), ftype);
    localArgs.insert(localArgs.end(), {dim, result});
    builder.create<fir::CallOp>(loc, funcOp, localArgs);
    return builder.create<fir::LoadOp>(loc, result);
  }
}

/// Generate Call to runtime prif_ucobound_{with|no}_dim
fir::ExtendedValue fir::runtime::genUCoBounds(fir::FirOpBuilder &builder,
                                              mlir::Location loc,
                                              mlir::Value handle, size_t corank,
                                              mlir::Value dim) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());

  mlir::func::FuncOp funcOp;
  llvm::SmallVector<mlir::Value> localArgs = {handle};
  if (isStaticallyAbsent(dim)) {
    llvm::SmallVector<mlir::Value, 1> extents{
        builder.createIntegerConstant(loc, builder.getIndexType(), corank)};
    mlir::Type resultType = fir::SequenceType::get(
        static_cast<fir::SequenceType::Extent>(corank), builder.getI64Type());
    mlir::Value result =
        builder.createBox(loc, builder.createTemporary(loc, resultType));
    mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy);
    funcOp =
        builder.createFunction(loc, PRIFNAME_SUB("ucobound_no_dim"), ftype);
    localArgs.emplace_back(result);
    builder.create<fir::CallOp>(loc, funcOp, localArgs);
    return fir::ArrayBoxValue(result, extents);
  } else {
    mlir::Value result = builder.createTemporary(loc, builder.getI64Type());
    mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy);
    funcOp =
        builder.createFunction(loc, PRIFNAME_SUB("ucobound_with_dim"), ftype);
    localArgs.insert(localArgs.end(), {dim, result});
    builder.create<fir::CallOp>(loc, funcOp, localArgs);
    return builder.create<fir::LoadOp>(loc, result);
  }
}

/// Generate Call to runtime prif_coshape
mlir::Value fir::runtime::genCoshape(fir::FirOpBuilder &builder,
                                     mlir::Location loc, mlir::Value handle,
                                     size_t corank) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::Type resultType = fir::SequenceType::get(
      static_cast<fir::SequenceType::Extent>(corank), builder.getI64Type());
  mlir::Value result =
      builder.createBox(loc, builder.createTemporary(loc, resultType));

  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("coshape"), ftype);
  llvm::SmallVector<mlir::Value> localArgs = {handle, result};
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
  return result;
}

/// Generate Call to runtime prif_size_bytes from any fortran value/entity
// and try to get the coarray_handle from this variable.
mlir::Value fir::runtime::genSizeBytes(fir::FirOpBuilder &builder,
                                       mlir::Location loc, mlir::Value A) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("size_bytes"), ftype);

  mlir::Value result = builder.createTemporary(loc, builder.getI64Type());
  mlir::Value coarrayHandle = fir::runtime::getCoarrayHandle(builder, loc, A);
  llvm::SmallVector<mlir::Value> localArgs =
      fir::runtime::createArguments(builder, loc, ftype, coarrayHandle, result);
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
  return result;
}

/// Generate call to runtime subroutine prif_get to fetches data in a
/// coarray from a specified image when data to be copied are contiguous in
/// memory from both sides.
void fir::runtime::CoarrayGet(fir::FirOpBuilder &builder, mlir::Location loc,
                              mlir::Value imageNum, mlir::Value handle,
                              mlir::Value offset,
                              mlir::Value currentImageBuffer,
                              mlir::Value sizeInBytes) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype =
      PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy, ptrTy, ptrTy, ptrTy, ptrTy, ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("get"), ftype);

  mlir::Value nullPtr = builder.createNullConstant(loc);
  llvm::SmallVector<mlir::Value> localArgs = {
      imageNum,    handle,  offset,  currentImageBuffer,
      sizeInBytes, nullPtr, nullPtr, nullPtr};
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
}

/// Generate call to runtime subroutine prif_get_stridded
void fir::runtime::CoarrayGetStrided(
    fir::FirOpBuilder &builder, mlir::Location loc, mlir::Value imageNum,
    mlir::Value handle, mlir::Value offset, mlir::Value remoteStride,
    mlir::Value currentImageBuffer, mlir::Value currentImageStride,
    mlir::Value elementSize, mlir::Value extent) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype =
      PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy, ptrTy, ptrTy, ptrTy, ptrTy, ptrTy,
                    ptrTy, ptrTy, ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("get_strided"), ftype);

  mlir::Value nullPtr = builder.createNullConstant(loc);
  llvm::SmallVector<mlir::Value> localArgs = {imageNum,
                                              handle,
                                              offset,
                                              remoteStride,
                                              currentImageBuffer,
                                              currentImageStride,
                                              elementSize,
                                              extent,
                                              nullPtr,
                                              nullPtr,
                                              nullPtr};
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
}

/// Generate call to runtime subroutine prif_put to assigns to elements of a
/// coarray from a specified image when data to be assigned are contiguous in
/// memory from both sides.
void fir::runtime::CoarrayPut(fir::FirOpBuilder &builder, mlir::Location loc,
                              mlir::Value imageNum, mlir::Value handle,
                              mlir::Value offset,
                              mlir::Value currentImageBuffer,
                              mlir::Value sizeInBytes) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype =
      PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy, ptrTy, ptrTy, ptrTy, ptrTy, ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("put"), ftype);

  mlir::Value nullPtr = builder.createNullConstant(loc);
  llvm::SmallVector<mlir::Value> localArgs = {
      imageNum,    handle,  offset,  currentImageBuffer,
      sizeInBytes, nullPtr, nullPtr, nullPtr};
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
}

/// Generate call to runtime subroutine prif_put to assigns to elements of a
/// coarray from a specified image when data to be assigned are contiguous in
/// memory from both sides.
void fir::runtime::CoarrayPutStrided(
    fir::FirOpBuilder &builder, mlir::Location loc, mlir::Value imageNum,
    mlir::Value handle, mlir::Value offset, mlir::Value remoteStride,
    mlir::Value currentImageBuffer, mlir::Value currentImageStride,
    mlir::Value elementSize, mlir::Value extent) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype =
      PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy, ptrTy, ptrTy, ptrTy, ptrTy, ptrTy,
                    ptrTy, ptrTy, ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("put_strided"), ftype);

  mlir::Value nullPtr = builder.createNullConstant(loc);
  llvm::SmallVector<mlir::Value> localArgs = {imageNum,
                                              handle,
                                              offset,
                                              remoteStride,
                                              currentImageBuffer,
                                              currentImageStride,
                                              elementSize,
                                              extent,
                                              nullPtr,
                                              nullPtr,
                                              nullPtr};
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
}

/// Generate call to runtime subroutine prif_sync_all
void fir::runtime::genSyncAllStatement(fir::FirOpBuilder &builder,
                                       mlir::Location loc, mlir::Value stat,
                                       mlir::Value errmsg) {
  mlir::Value nullPtr = builder.createNullConstant(loc);
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("sync_all"), ftype);

  llvm::SmallVector<mlir::Value> localArgs = {stat, errmsg, nullPtr};
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
}

/// Generate call to runtime subroutine prif_sync_memory
void fir::runtime::genSyncMemoryStatement(fir::FirOpBuilder &builder,
                                          mlir::Location loc, mlir::Value stat,
                                          mlir::Value errmsg) {
  mlir::Value nullPtr = builder.createNullConstant(loc);
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("sync_memory"), ftype);

  llvm::SmallVector<mlir::Value> localArgs = {stat, errmsg, nullPtr};
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
}

/// Generate call to runtime subroutine prif_sync_images
void fir::runtime::genSyncImagesStatement(fir::FirOpBuilder &builder,
                                          mlir::Location loc,
                                          mlir::Value imageSet,
                                          mlir::Value stat,
                                          mlir::Value errmsg) {
  mlir::Value nullPtr = builder.createNullConstant(loc);
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy, ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("sync_images"), ftype);

  llvm::SmallVector<mlir::Value> localArgs = {imageSet, stat, errmsg, nullPtr};
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
}

/// Generate call to runtime subroutine prif_sync_team
void fir::runtime::genSyncTeamStatement(fir::FirOpBuilder &builder,
                                        mlir::Location loc, mlir::Value team,
                                        mlir::Value stat, mlir::Value errmsg) {
  mlir::Value nullPtr = builder.createNullConstant(loc);
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy, ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("sync_team"), ftype);

  llvm::SmallVector<mlir::Value> localArgs = {team, stat, errmsg, nullPtr};
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
}

/// Generate call to runtime subroutine prif_lock
void fir::runtime::genLockStatement(fir::FirOpBuilder &builder,
                                    mlir::Location loc, mlir::Value imageNum,
                                    mlir::Value handle,
                                    mlir::Value acquiredLock,
                                    mlir::Value offset, mlir::Value stat,
                                    mlir::Value errmsg) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype =
      PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy, ptrTy, ptrTy, ptrTy, ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("lock"), ftype);

  mlir::Value nullPtr = builder.createNullConstant(loc);
  llvm::SmallVector<mlir::Value> localArgs = {
      imageNum, handle, acquiredLock, offset, stat, errmsg, nullPtr};
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
}

/// Generate call to runtime subroutine prif_unlock
void fir::runtime::genUnlockStatement(fir::FirOpBuilder &builder,
                                      mlir::Location loc, mlir::Value imageNum,
                                      mlir::Value handle, mlir::Value offset,
                                      mlir::Value stat, mlir::Value errmsg) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype =
      PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy, ptrTy, ptrTy, ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("unlock"), ftype);

  mlir::Value nullPtr = builder.createNullConstant(loc);
  llvm::SmallVector<mlir::Value> localArgs = {imageNum, handle, offset,
                                              stat,     errmsg, nullPtr};
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
}

/// Generate Call to runtime prif_fail_image
void fir::runtime::genFailImageStatement(fir::FirOpBuilder &builder,
                                         mlir::Location loc) {
  mlir::FunctionType ftype =
      mlir::FunctionType::get(builder.getContext(), {}, {});
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("fail_image"), ftype);
  builder.create<fir::CallOp>(loc, funcOp);
}

/// Generate call to collective subroutines except co_reduce_
/// A must be lowered as a box
void genCollectiveSubroutine(fir::FirOpBuilder &builder, mlir::Location loc,
                             mlir::Value A, mlir::Value sourceImage,
                             mlir::Value stat, mlir::Value errmsg,
                             std::string coName) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::Type boxNoneTy = fir::BoxType::get(builder.getNoneType());
  mlir::FunctionType ftype =
      PRIF_FUNCTYPE(boxNoneTy, ptrTy, ptrTy, ptrTy, ptrTy);
  mlir::func::FuncOp funcOp = builder.createFunction(loc, coName, ftype);

  mlir::Value nullPtr = builder.createNullConstant(loc);
  llvm::SmallVector<mlir::Value> localArgs = {A, sourceImage, stat, nullPtr,
                                              errmsg};
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
}
/// Generate call to runtime subroutine prif_co_broadcast
void fir::runtime::genCoBroadcast(fir::FirOpBuilder &builder,
                                  mlir::Location loc, mlir::Value A,
                                  mlir::Value sourceImage, mlir::Value stat,
                                  mlir::Value errmsg) {
  genCollectiveSubroutine(builder, loc, A, sourceImage, stat, errmsg,
                          PRIFNAME_SUB("co_broadcast"));
}

/// Generate call to runtime subroutine prif_co_max or prif_co_max_character
void fir::runtime::genCoMax(fir::FirOpBuilder &builder, mlir::Location loc,
                            mlir::Value A, mlir::Value resultImage,
                            mlir::Value stat, mlir::Value errmsg) {
  if (fir::unwrapPassByRefType(A.getType()).isInteger(8)) {
    // FIXME: Need to embox A into a CharBoxValue or CharArrayBoxValue ?
    genCollectiveSubroutine(builder, loc, A, resultImage, stat, errmsg,
                            PRIFNAME_SUB("co_max_character"));
  } else {
    genCollectiveSubroutine(builder, loc, A, resultImage, stat, errmsg,
                            PRIFNAME_SUB("co_max"));
  }
}

/// Generate call to runtime subroutine prif_co_min or prif_co_min_character
void fir::runtime::genCoMin(fir::FirOpBuilder &builder, mlir::Location loc,
                            mlir::Value A, mlir::Value resultImage,
                            mlir::Value stat, mlir::Value errmsg) {
  if (fir::unwrapPassByRefType(A.getType()).isInteger(8)) {
    // FIXME: Need to embox A into a CharBoxValue or CharArrayBoxValue ?
    genCollectiveSubroutine(builder, loc, A, resultImage, stat, errmsg,
                            PRIFNAME_SUB("co_min_character"));
  } else {
    genCollectiveSubroutine(builder, loc, A, resultImage, stat, errmsg,
                            PRIFNAME_SUB("co_min"));
  }
}

/// Generate call to runtime subroutine prif_get_context
mlir::Value getContextData(fir::FirOpBuilder &builder, mlir::Location loc,
                           mlir::Value coarrayHandle) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy);
  mlir::func::FuncOp funcOp = builder.createFunction(loc, "get_context", ftype);

  mlir::Value cdata = builder.createTemporary(loc, ptrTy);
  llvm::SmallVector<mlir::Value> localArgs =
      fir::runtime::createArguments(builder, loc, ftype, coarrayHandle, cdata);
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
  return builder.create<fir::LoadOp>(loc, cdata).getResult();
}

void genOperationWrapperRuntimeCall(fir::FirOpBuilder &builder,
                                    mlir::Location loc,
                                    mlir::func::FuncOp funcOp, mlir::Value arg1,
                                    mlir::Value arg2_and_out, mlir::Type baseTy,
                                    mlir::Value addr_arg2_and_out = {}) {
  llvm::SmallVector<mlir::Value> opArgs;
  mlir::Type resultType = funcOp.getFunctionType().getResult(0);
  // Create arguments list
  if (auto boxCharType = mlir::dyn_cast<fir::BoxCharType>(baseTy)) {
    mlir::Type lenType = builder.getCharacterLengthType();
    auto refType = builder.getRefType(boxCharType.getEleTy());
    auto unboxed =
        builder.create<fir::UnboxCharOp>(loc, refType, lenType, arg2_and_out);
    mlir::Value ref_arg2 =
        builder.createConvert(loc, refType, unboxed.getResult(0));
    auto unboxed_len = unboxed.getResult(1);
    opArgs = fir::runtime::createArguments(builder, loc,
                                           funcOp.getFunctionType(), ref_arg2,
                                           unboxed_len, arg1, arg2_and_out);
  } else {
    opArgs = fir::runtime::createArguments(
        builder, loc, funcOp.getFunctionType(), arg1, arg2_and_out);
  }

  mlir::Value result =
      builder.create<fir::CallOp>(loc, funcOp, opArgs).getResult(0);

  // Storing result
  if (!mlir::dyn_cast<fir::BoxCharType>(resultType)) {
    if (!isStaticallyAbsent(addr_arg2_and_out))
      builder.create<fir::StoreOp>(loc, result, addr_arg2_and_out);
    else
      builder.create<fir::StoreOp>(loc, result, arg2_and_out);
  }
}

// Generate operation wrapper just like describe in PRIF specification
mlir::Value genOperationWrapper(fir::FirOpBuilder &builder, mlir::Location loc,
                                mlir::Value operation) {
  // Character procedure tuple
  if (auto extractValue = mlir::dyn_cast_or_null<fir::ExtractValueOp>(
          operation.getDefiningOp())) {
    auto insertVal1 = mlir::dyn_cast<fir::InsertValueOp>(
        extractValue.getAdt().getDefiningOp());
    auto insertVal2 =
        mlir::dyn_cast<fir::InsertValueOp>(insertVal1.getAdt().getDefiningOp());
    operation = insertVal2.getVal();
  }

  // Getting originel funcOp
  mlir::func::FuncOp oldFuncOp;
  mlir::Type argTy, elemTy;
  mlir::ModuleOp module =
      operation.getDefiningOp()->getParentOfType<mlir::ModuleOp>();
  if (auto embox =
          mlir::dyn_cast_or_null<fir::EmboxProcOp>(operation.getDefiningOp())) {
    auto addrOfOp =
        mlir::dyn_cast<fir::AddrOfOp>(embox.getFunc().getDefiningOp());
    mlir::SymbolRefAttr symbolAttr = addrOfOp.getSymbolAttr();
    oldFuncOp = module.lookupSymbol<mlir::func::FuncOp>(symbolAttr);
    argTy = oldFuncOp.getFunctionType().getResult(0);
    if (!fir::isa_box_type(argTy) && !fir::isa_ref_type(argTy))
      argTy = builder.getRefType(argTy);
    elemTy = fir::unwrapRefType(argTy);
  }

  // Declaration of the new wrapper function operation
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType funcType = mlir::FunctionType::get(
      builder.getContext(),
      /*inputs*/ {argTy, argTy, builder.getI64Type(), ptrTy},
      /*result*/ {});
  auto funcName = "co_reduce_operation_wrapper_" + oldFuncOp.getName();
  mlir::func::FuncOp funcOp =
      module.lookupSymbol<mlir::func::FuncOp>(funcName.str());
  if (!funcOp) { // new function
    funcOp = builder.createFunction(loc, funcName.str(), funcType);

    // generating the body of the function.
    mlir::OpBuilder::InsertPoint saveInsertPoint = builder.saveInsertionPoint();
    builder.setInsertionPointToStart(funcOp.addEntryBlock());

    auto args = funcOp.getArguments();
    mlir::Value arg1 = args[0];
    mlir::Value arg2_and_out = args[1];
    mlir::Value count = args[2];

    mlir::IndexType idxTy = builder.getIndexType();
    if (mlir::isa<fir::SequenceType>(arg1.getType())) {
      mlir::Value one = builder.createIntegerConstant(loc, idxTy, 1);
      auto loop = builder.create<fir::DoLoopOp>(loc, one, count, /*step*/ one);

      // Begin Loop code
      mlir::OpBuilder::InsertPoint loopEndPt = builder.saveInsertionPoint();
      builder.setInsertionPointToStart(loop.getBody());
      mlir::Value index = loop.getInductionVar();
      mlir::Value addr1 =
          builder.create<fir::CoordinateOp>(loc, elemTy, arg1, index);
      mlir::Value elem1 = builder.create<fir::LoadOp>(loc, addr1);
      mlir::Value addr2 =
          builder.create<fir::CoordinateOp>(loc, elemTy, arg2_and_out, index);
      mlir::Value elem2 = builder.create<fir::LoadOp>(loc, addr2);

      genOperationWrapperRuntimeCall(builder, loc, oldFuncOp, elem1, elem2,
                                     argTy, addr2);

      // End of loop
      builder.restoreInsertionPoint(loopEndPt);
    } else {
      genOperationWrapperRuntimeCall(builder, loc, oldFuncOp, arg1,
                                     arg2_and_out, argTy);
    }
    builder.create<mlir::func::ReturnOp>(loc);
    builder.restoreInsertionPoint(saveInsertPoint);
  }

  mlir::SymbolRefAttr symbolRef =
      mlir::SymbolRefAttr::get(builder.getContext(), funcOp.getSymNameAttr());
  mlir::Value addrOfOp =
      builder.create<fir::AddrOfOp>(loc, funcType, symbolRef);
  mlir::Type boxTy = fir::BoxProcType::get(builder.getContext(), funcType);
  mlir::Value boxproc = builder.create<fir::EmboxProcOp>(loc, boxTy, addrOfOp);
  mlir::Value refBoxProc =
      builder.create<fir::AllocaOp>(loc, boxproc.getType());
  builder.create<fir::StoreOp>(loc, boxproc, refBoxProc);
  return refBoxProc;
}

/// Generate call to runtime subroutine prif_co_reduce
void fir::runtime::genCoReduce(fir::FirOpBuilder &builder, mlir::Location loc,
                               mlir::Value A, mlir::Value operation,
                               mlir::Value resultImage, mlir::Value stat,
                               mlir::Value errmsg) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::Type boxNoneTy = fir::BoxType::get(builder.getNoneType());
  mlir::FunctionType ftype =
      PRIF_FUNCTYPE(boxNoneTy, ptrTy, ptrTy, ptrTy, ptrTy, ptrTy, boxNoneTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("co_reduce"), ftype);

  mlir::Value nullPtr = builder.createNullConstant(loc);
  mlir::Value opWrapper = genOperationWrapper(builder, loc, operation);
  llvm::SmallVector<mlir::Value> localArgs = fir::runtime::createArguments(
      builder, loc, ftype, A, opWrapper, /*cdata*/ nullPtr, resultImage, stat,
      nullPtr, errmsg);
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
}

/// Generate call to runtime subroutine prif_co_sum_
void fir::runtime::genCoSum(fir::FirOpBuilder &builder, mlir::Location loc,
                            mlir::Value A, mlir::Value resultImage,
                            mlir::Value stat, mlir::Value errmsg) {
  genCollectiveSubroutine(builder, loc, A, resultImage, stat, errmsg,
                          PRIFNAME_SUB("co_sum"));
}

/// Generate call to runtime subroutine prif_form_team
void fir::runtime::genFormTeamStatement(fir::FirOpBuilder &builder,
                                        mlir::Location loc,
                                        mlir::Value teamNumber,
                                        mlir::Value team, mlir::Value newIndex,
                                        mlir::Value stat, mlir::Value errMsg) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype =
      PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy, ptrTy, ptrTy, ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("form_team"), ftype);

  mlir::Value none = builder.create<fir::AbsentOp>(
      loc, fir::BoxType::get(mlir::NoneType::get(builder.getContext())));
  llvm::SmallVector<mlir::Value> localArgs = {teamNumber, team, newIndex,
                                              stat,       none, errMsg};
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
}

/// Generate call to runtime subroutine prif_change_team
void fir::runtime::genChangeTeamStatement(fir::FirOpBuilder &builder,
                                          mlir::Location loc, mlir::Value team,
                                          mlir::Value stat,
                                          mlir::Value errMsg) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy, ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("change_team"), ftype);

  mlir::Value none = builder.create<fir::AbsentOp>(
      loc, fir::BoxType::get(mlir::NoneType::get(builder.getContext())));
  llvm::SmallVector<mlir::Value> localArgs = {team, stat, none, errMsg};
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
}

/// Generate call to runtime subroutine prif_end_team
void fir::runtime::genEndTeamStatement(fir::FirOpBuilder &builder,
                                       mlir::Location loc, mlir::Value stat,
                                       mlir::Value errMsg) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("end_team"), ftype);

  mlir::Value none = builder.create<fir::AbsentOp>(
      loc, fir::BoxType::get(mlir::NoneType::get(builder.getContext())));
  llvm::SmallVector<mlir::Value> localArgs = {stat, none, errMsg};
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
}

/// Generate call to runtime subroutine prif_get_team
mlir::Value fir::runtime::genGetTeam(fir::FirOpBuilder &builder,
                                     mlir::Location loc, mlir::Value level) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("get_team"), ftype);

  // Handle TEAM as result of prif_get_team
  mlir::Type noneTy =
      fir::BoxType::get(mlir::NoneType::get(builder.getContext()));
  mlir::Value team =
      builder.createBox(loc, builder.createTemporary(loc, noneTy));

  llvm::SmallVector<mlir::Value> localArgs = {level, team};
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
  return team;
}

/// Generate call to runtime subroutine prif_team_number
mlir::Value fir::runtime::genTeamNumber(fir::FirOpBuilder &builder,
                                        mlir::Location loc, mlir::Value team) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("team_number"), ftype);

  // Handle TEAM-NUMBER as result of prif_team_number
  mlir::Value result = builder.createTemporary(loc, builder.getI64Type());

  llvm::SmallVector<mlir::Value> localArgs = {team, result};
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
  return builder.create<fir::LoadOp>(loc, result);
}

/// Generate call to runtime subroutine prif_atomic_cas_{int|logical}
void fir::runtime::genAtomicCas(fir::FirOpBuilder &builder, mlir::Location loc,
                                mlir::Value imageNum, mlir::Value handle,
                                mlir::Value offset, mlir::Value old,
                                mlir::Value compare, mlir::Value newV,
                                mlir::Value stat) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy, ptrTy, ptrTy);
  bool isLogicalType = fir::getBaseTypeOf(newV).isInteger(1);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc,
                             isLogicalType ? PRIFNAME_SUB("atomic_cas_logical")
                                           : PRIFNAME_SUB("atomic_cas_int"),
                             ftype);
  llvm::SmallVector<mlir::Value> localArgs = {imageNum, handle, offset, old,
                                              compare,  newV,   stat};
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
}

/// Generate call to runtime subroutine prif_atomic_define_{int|logical}
void fir::runtime::genAtomicDefine(fir::FirOpBuilder &builder,
                                   mlir::Location loc, mlir::Value imageNum,
                                   mlir::Value handle, mlir::Value offset,
                                   mlir::Value value, mlir::Value stat) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy, ptrTy, ptrTy);
  bool isLogicalType = fir::getBaseTypeOf(value).isInteger(1);
  mlir::func::FuncOp funcOp = builder.createFunction(
      loc,
      isLogicalType ? PRIFNAME_SUB("atomic_define_logical")
                    : PRIFNAME_SUB("atomic_define_int"),
      ftype);
  llvm::SmallVector<mlir::Value> localArgs = {imageNum, handle, offset, value,
                                              stat};
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
}

/// Generate call to runtime subroutine prif_atomic_[fetch_]{add, and, or, xor}
/// "value": Need to be lowered into a BoxValue.
void fir::runtime::genAtomicOp(fir::FirOpBuilder &builder, mlir::Location loc,
                               mlir::Value imageNum, mlir::Value handle,
                               mlir::Value offset, mlir::Value value,
                               mlir::Value old, mlir::Value stat, int opKind,
                               bool isFetch) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype =
      isFetch ? PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy, ptrTy, ptrTy, ptrTy)
              : PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy, ptrTy, ptrTy);
  mlir::func::FuncOp funcOp;
  switch (opKind) {
  case ATOMIC_ADD:
    funcOp = builder.createFunction(loc,
                                    isFetch ? PRIFNAME_SUB("atomic_fetch_add")
                                            : PRIFNAME_SUB("atomic_add"),
                                    ftype);
    break;
  case ATOMIC_AND:
    funcOp = builder.createFunction(loc,
                                    isFetch ? PRIFNAME_SUB("atomic_fetch_and")
                                            : PRIFNAME_SUB("atomic_and"),
                                    ftype);
    break;
  case ATOMIC_OR:
    funcOp = builder.createFunction(loc,
                                    isFetch ? PRIFNAME_SUB("atomic_fetch_or")
                                            : PRIFNAME_SUB("atomic_or"),
                                    ftype);
    break;
  case ATOMIC_XOR:
    funcOp = builder.createFunction(loc,
                                    isFetch ? PRIFNAME_SUB("atomic_fetch_xor")
                                            : PRIFNAME_SUB("atomic_xor"),
                                    ftype);
    break;
  default:
    llvm::errs() << "Unsupported atomic operation\n.";
  }

  llvm::SmallVector<mlir::Value> localArgs;
  if (isFetch)
    localArgs.insert(localArgs.end(),
                     {imageNum, handle, offset, value, old, stat});
  else
    localArgs.insert(localArgs.end(), {imageNum, handle, offset, value, stat});
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
}

/// Generate call to runtime subroutine prif_atomic_ref_{int|logical}
void fir::runtime::genAtomicRef(fir::FirOpBuilder &builder, mlir::Location loc,
                                mlir::Value imageNum, mlir::Value handle,
                                mlir::Value offset, mlir::Value value,
                                mlir::Value stat) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy, ptrTy, ptrTy);
  bool isLogicalType = fir::getBaseTypeOf(value).isInteger(1);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc,
                             isLogicalType ? PRIFNAME_SUB("atomic_ref_logical")
                                           : PRIFNAME_SUB("atomic_ref_int"),
                             ftype);
  llvm::SmallVector<mlir::Value> localArgs = {imageNum, handle, offset, value,
                                              stat};
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
}

/// Generate call to runtime subroutine prif_event_post
void fir::runtime::genEventPostStatement(fir::FirOpBuilder &builder,
                                         mlir::Location loc,
                                         mlir::Value imageNum,
                                         mlir::Value handle, mlir::Value offset,
                                         mlir::Value stat, mlir::Value errmsg) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype =
      PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy, ptrTy, ptrTy, ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("event_post"), ftype);

  mlir::Value nullPtr = builder.createNullConstant(loc);
  llvm::SmallVector<mlir::Value> localArgs = {imageNum, handle, offset,
                                              stat,     errmsg, nullPtr};
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
}

/// Generate call to runtime subroutine prif_event_wait
void fir::runtime::genEventWaitStatement(fir::FirOpBuilder &builder,
                                         mlir::Location loc,
                                         mlir::Value eventVarPtr,
                                         mlir::Value untilCount,
                                         mlir::Value stat, mlir::Value errmsg) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy, ptrTy, ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("event_wait"), ftype);

  mlir::Value nullPtr = builder.createNullConstant(loc);
  llvm::SmallVector<mlir::Value> localArgs = {eventVarPtr, untilCount, stat,
                                              nullPtr, errmsg};
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
}

/// Generate call to runtime subroutine prif_notify_wait
void fir::runtime::genNotifyWaitStatement(
    fir::FirOpBuilder &builder, mlir::Location loc, mlir::Value notifyVarPtr,
    mlir::Value untilCount, mlir::Value stat, mlir::Value errmsg) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy, ptrTy, ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("notify_wait"), ftype);

  mlir::Value nullPtr = builder.createNullConstant(loc);
  llvm::SmallVector<mlir::Value> localArgs = {notifyVarPtr, untilCount, stat,
                                              nullPtr, errmsg};
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
}

/// Generate call to runtime subroutine prif_event_query
void fir::runtime::genEventQuery(fir::FirOpBuilder &builder, mlir::Location loc,
                                 mlir::Value eventVarPtr, mlir::Value count,
                                 mlir::Value stat) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("event_query"), ftype);

  llvm::SmallVector<mlir::Value> localArgs = {eventVarPtr, count, stat};
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
}

/// Generate call to runtime subroutine prif_alias_create
mlir::Value fir::runtime::genAliasCreate(fir::FirOpBuilder &builder,
                                         mlir::Location loc,
                                         mlir::Value sourceHandle,
                                         mlir::Value aliasLcobounds,
                                         mlir::Value aliasUcobounds,
                                         mlir::Type handleTy) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy, ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("alias_create"), ftype);

  // Allocate instance of prif_coarray_handle type.
  mlir::Value aliasHandle =
      builder.createBox(loc, builder.createTemporary(loc, handleTy));

  llvm::SmallVector<mlir::Value> localArgs = fir::runtime::createArguments(
      builder, loc, ftype, sourceHandle, aliasLcobounds, aliasUcobounds,
      aliasHandle);
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
  return aliasHandle;
}

/// Generate call to runtime subroutine prif_alias_destroy
void fir::runtime::genAliasDestroy(fir::FirOpBuilder &builder,
                                   mlir::Location loc,
                                   mlir::Value aliasHandle) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("alias_destroy"), ftype);

  llvm::SmallVector<mlir::Value> localArgs =
      fir::runtime::createArguments(builder, loc, ftype, aliasHandle);
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
}

/// Generate call to runtime subroutine prif_critical
void fir::runtime::genCriticalStatement(fir::FirOpBuilder &builder,
                                        mlir::Location loc,
                                        mlir::Value coarrayHandle,
                                        mlir::Value stat, mlir::Value errmsg) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy, ptrTy, ptrTy, ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("critical"), ftype);

  mlir::Value nullPtr = builder.createNullConstant(loc);
  llvm::SmallVector<mlir::Value> localArgs = fir::runtime::createArguments(
      builder, loc, ftype, coarrayHandle, stat, errmsg, nullPtr);
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
}

/// Generate call to runtime subroutine prif_end_critical
void fir::runtime::genEndCriticalStatement(fir::FirOpBuilder &builder,
                                           mlir::Location loc,
                                           mlir::Value coarrayHandle) {
  mlir::Type ptrTy = fir::PointerType::get(builder.getNoneType());
  mlir::FunctionType ftype = PRIF_FUNCTYPE(ptrTy);
  mlir::func::FuncOp funcOp =
      builder.createFunction(loc, PRIFNAME_SUB("end_critical"), ftype);

  llvm::SmallVector<mlir::Value> localArgs =
      fir::runtime::createArguments(builder, loc, ftype, coarrayHandle);
  builder.create<fir::CallOp>(loc, funcOp, localArgs);
}
