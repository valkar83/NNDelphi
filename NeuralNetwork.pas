unit NeuralNetwork;

interface
uses
  Matrix,
  System.Types,
  System.Generics.Collections;
type
  TNeuralNetwork = CLASS (TObject)
    FListeNeuroneParCouche : TList<Integer>;
    FNbCouches             : Integer;
    FBiais                 : Array of TDoubleMatrix;
    FCoefficients          : Array of TDoubleMatrix;

    constructor Init(AListeNeuroneParCouche : TList<Integer>);
  end;
implementation
USes
  RandomEng;
{ TNeuralNetwork }

constructor TNeuralNetwork.Init(AListeNeuroneParCouche: TList<Integer>);
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
    Inc(LJ);
  end;


end;

end.
