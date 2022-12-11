unit NeuralNetwork;

interface
uses
  Math,
  Matrix,
  System.Types,
  System.Generics.Collections;
type
  TNeuralNetwork = CLASS (TObject)
    FNbCouches             : Integer;
    FBiais                 : Array of TDoubleMatrix;
    FCoefficients          : Array of TDoubleMatrix;

    constructor Init(CONST AListeNeuroneParCouche : TList<Integer>);
    FUNCTION Sigmoid(Z : TDoubleMatrix) : TDoubleMatrix;
  private
    FListeNeuroneParCouche : TList<Integer>;
    procedure prendreExponentiel(var Value: double);
    procedure prendreInverse(var Value: double);
  end;
implementation
USes
  MtxUtilFunc,
  RandomEng;
{ TNeuralNetwork }

constructor TNeuralNetwork.Init(CONST AListeNeuroneParCouche: TList<Integer>);
var
  LI,
  LJ  : Integer;
begin
  Inherited Create;
  FListeNeuroneParCouche := AListeNeuroneParCouche;
  FNbCouches             := AListeNeuroneParCouche.Count;

  setLength(FCoefficients, FListeNeuroneParCouche.Count - 1);
  setLength(FBiais       , FListeNeuroneParCouche.Count - 1);

  for LI := 1 to (FListeNeuroneParCouche.Count - 1) do
    FBiais[LI] := TDoubleMatrix.CreateRand(1, FListeNeuroneParCouche[LI], raSystem, 0);

  for LI := 1 to (FListeNeuroneParCouche.Count - 1) do
  begin
    LJ := LI - 1;
    FBiais[LI] := TDoubleMatrix.CreateRand(FListeNeuroneParCouche[LJ],
                                           FListeNeuroneParCouche[LI], raSystem, 0);
  end;
  Sigmoid(FBiais[FListeNeuroneParCouche.Count - 1]);



end;
procedure TNeuralNetwork.prendreExponentiel(var Value: double);
begin
  Value := exp(Value);
end;

PROCEDURE TNeuralNetwork.prendreInverse(var Value: double);
begin
  Value := 1 / Value;
end;
function TNeuralNetwork.Sigmoid(Z: TDoubleMatrix): TDoubleMatrix;

begin
  MatrixToTxtFile('Etat_initial', Z);
  result := z.ElementwiseFunc(prendreExponentiel);
  MatrixToTxtFile('Exponentiel', Result);
  result := result.add(1);
  MatrixToTxtFile('1 plus Exponentiel', Result);
  result := result.ElementwiseFunc(prendreInverse);
  MatrixToTxtFile('1 div 1 plus Exponentiel', Result);
end;

end.
