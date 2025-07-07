//===-- Lower/Coarray.h -- image related lowering ---------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef FORTRAN_LOWER_COARRAY_H
#define FORTRAN_LOWER_COARRAY_H

#include "flang/Lower/AbstractConverter.h"
#include "flang/Optimizer/Builder/BoxValue.h"

namespace Fortran {

namespace parser {
struct ChangeTeamConstruct;
struct ChangeTeamStmt;
struct EndChangeTeamStmt;
struct FormTeamStmt;
struct CriticalStmt;
struct EndCriticalStmt;
} // namespace parser

namespace evaluate {
class CoarrayRef;
} // namespace evaluate

namespace lower {

class SymMap;

namespace pft {
struct Evaluation;
} // namespace pft

//===----------------------------------------------------------------------===//
// TEAM constructs
//===----------------------------------------------------------------------===//

void genChangeTeamConstruct(AbstractConverter &, pft::Evaluation &eval,
                            const parser::ChangeTeamConstruct &);
void genChangeTeamStmt(AbstractConverter &, pft::Evaluation &eval,
                       const parser::ChangeTeamStmt &);
void genEndChangeTeamStmt(AbstractConverter &, pft::Evaluation &eval,
                          const parser::EndChangeTeamStmt &);
void genFormTeamStatement(AbstractConverter &, pft::Evaluation &eval,
                          const parser::FormTeamStmt &);

//===----------------------------------------------------------------------===//
// CRITICAL construct
//===----------------------------------------------------------------------===//

mlir::Value genCriticalCorarrayHandle(AbstractConverter &);
void genCriticalStmt(AbstractConverter &, pft::Evaluation &eval,
                     const parser::CriticalStmt &, mlir::Value coarrayAddr);
void genEndCriticalStmt(AbstractConverter &, pft::Evaluation &eval,
                        const parser::EndCriticalStmt &,
                        mlir::Value coarrayAddr);

//===----------------------------------------------------------------------===//
// COARRAY utils 
//===----------------------------------------------------------------------===//

/// Check if CoarrayFeature from LoweringOptions is enabled.
/// Emit an error if not.
void checkCoarrayFeatureEnabled(AbstractConverter &converter,
                                mlir::Location loc);

// Compute the colower and coupper bounds for a coarray and return an array of
// bounds.
std::pair<mlir::Value, mlir::Value>
genCoarrayCoBounds(AbstractConverter &converter, mlir::Location loc,
                   const semantics::Symbol &sym);

/// From coarray expression, generate vectors of mlir::Value with cosubscripts
llvm::SmallVector<mlir::Value, 4> genCoshape(AbstractConverter &converter,
                                             mlir::Location loc,
                                             const SomeExpr &expr,
                                             StatementContext &stmtCtx);

/// From coarray expression, generate vector of cosubscript 
llvm::SmallVector<mlir::Value, 4>
genCoSubscripts(AbstractConverter &converter, mlir::Location loc,
            const SomeExpr &expr, StatementContext &stmtCtx);

/// From cosubscript, generate call to runtime function prif_image_index
/// associated to an addr
mlir::Value getImageIndexFromCosubscripts(
    Fortran::lower::AbstractConverter &converter, mlir::Location loc,
    const Fortran::evaluate::CoarrayRef &expr, mlir::Value coarrayAddr);

mlir::Value getImageIndexFromBox(fir::FirOpBuilder &builder, mlir::Location loc,
                                 fir::ExtendedValue box,
                                 mlir::Value handle = {});

mlir::Value genByteOffset(Fortran::lower::AbstractConverter &converter,
                          const Fortran::semantics::SomeExpr &expr,
                          mlir::Location loc);

mlir::Value getSizeInBytes(fir::FirOpBuilder &builder, mlir::Location loc,
                           fir::ExtendedValue box);

//===----------------------------------------------------------------------===//
// COARRAY memory management
//===----------------------------------------------------------------------===//

mlir::Type getCoarrayHandleType(fir::FirOpBuilder &builder, mlir::Location loc);

mlir::Value
genAllocateCoarrayRuntimeCall(fir::FirOpBuilder &builder, mlir::Location loc,
                              mlir::Value sizeInBytes, mlir::Value lcobounds,
                              mlir::Value ucobounds, mlir::Value allocMem,
                              mlir::Value stat, mlir::Value errMsg);

mlir::Value genAllocateCoarray(Fortran::lower::AbstractConverter &converter,
                               mlir::Location loc, const semantics::Symbol &sym,
                               fir::MutableBoxValue, mlir::Value errMsgAddr);

std::pair<mlir::Value, mlir::Value>
genAllocateCoarray(Fortran::lower::AbstractConverter &converter,
                   mlir::Location loc, const semantics::Symbol &sym,
                   fir::ExtendedValue preAlloc);

mlir::Value genDeallocateCoarray(Fortran::lower::AbstractConverter &converter,
                                 mlir::Location loc, fir::MutableBoxValue box,
                                 mlir::Value errMsgAddr);
mlir::Value genDeallocateCoarray(Fortran::lower::AbstractConverter &converter,
                                 mlir::Location loc, mlir::Value coarrayAddr,
                                 mlir::Value errMsgAddr);

//===----------------------------------------------------------------------===//
// COARRAY expressions
//===----------------------------------------------------------------------===//

/// Coarray expression lowering helper. A coarray expression is expected to be
/// lowered into runtime support calls. For example, expressions may use a
/// message-passing runtime to access another image's data.
class CoarrayExprHelper {
public:
  explicit CoarrayExprHelper(AbstractConverter &converter, mlir::Location loc,
                             SymMap &syms)
      : converter{converter}, symMap{syms}, loc{loc} {}
  CoarrayExprHelper(const CoarrayExprHelper &) = delete;

  /// Generate the address of a co-array expression.
  fir::ExtendedValue genAddr(const evaluate::CoarrayRef &expr);

  /// Generate the value of a co-array expression.
  fir::ExtendedValue genValue(const evaluate::CoarrayRef &expr);

private:
  AbstractConverter &converter;
  SymMap &symMap;
  mlir::Location loc;
};

} // namespace lower
} // namespace Fortran

#endif // FORTRAN_LOWER_COARRAY_H
