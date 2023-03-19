unit CoordDouble;

interface
uses
  Matrix,
  System.Types;
type
  TCoordDoubleMatrix = record
    X, Y    : IMatrix;
    YDouble : Double;
    PROCEDURE MisANilMatrix;
  end;

implementation

{ TCoordDoubleMatrix }

procedure TCoordDoubleMatrix.MisANilMatrix;
begin
  X := NIL;
  Y := NIL;
end;

end.
