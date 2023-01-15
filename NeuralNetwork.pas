unit NeuralNetwork;

interface
uses
  Math,
  Matrix,
  CoordDouble,
  System.Types,
  System.Generics.Collections;
type
  TNeuralNetwork = CLASS (TObject)
    FNbCouches             : Integer;
    FBiais                 : TArray<TDoubleMatrix>;
    FCoefficients          : TArray<TDoubleMatrix>;

    constructor Init(CONST AListeNeuroneParCouche : TList<Integer>);
    FUNCTION Sigmoid(Z : TDoubleMatrix) : TDoubleMatrix;
    FUNCTION Sigmoid_Prime(Z: TDoubleMatrix): TDoubleMatrix;
  private
    FListeNeuroneParCouche : TList<Integer>;
    FTrainingDate : TList<TCoordDouble>;
    Function fragmenterTList(AList : TList<TCoordDouble>; ATailleLot : Integer) : TList<TList<TCoordDouble>>;
    procedure libererTListTList(AListList : TList<TList<TCoordDouble>>);
    procedure shuffle(VAR AListeAMelanger : TList<TCoordDouble>);
    procedure prendreExponentiel(var Value: double);
    procedure feedforward(VAR A : TDoubleMatrix);
    procedure prendreInverse(var Value: double);
    procedure StochasticGradientDescent(ATrainingData      : TList<TCoordDouble>;
                                        AEta,    // Paramètre
                                        ANbPass, //EPoch en anglais
                                        ATailleDuMiniBatch : Integer;
                                        ATestData          : TList<TCoordDouble> = NIL);
    procedure mettreAJourLotDeData (ALotData : TList<TCoordDouble>;
                                    AEtat    : Integer);
    procedure BackPropagation(CONST AData : TCoordDouble;
                              out ADeltaNablaCoeff, ADeltaNablaBiais : TArray<TDoubleMatrix>);
    procedure remplirInitialiserTabMatrix(ATabMatrixARemplir,
      ATabMatrixRef: TArray<TDoubleMatrix>);
    Function cost_derivate(AResultatActivation, AY : TDoubleMatrix): TDoubleMatrix;


  end;
implementation
USes
  System.SysUtils,
  MtxUtilFunc,
  RandomEng;
{ TNeuralNetwork }

procedure TNeuralNetwork.BackPropagation(const AData                  : TCoordDouble;
                                         out ADeltaNablaCoeff, ADeltaNablaBiais : TArray<TDoubleMatrix>);
var
  LI : Integer;
  LZ,
  LSP,
  LDelta,
  LActivation,
  LCoeffiecientsTrans : TDoubleMatrix;
  LZs,
  LActivations : TArray<TDoubleMatrix>;
begin
  SetLength(LZs, FListeNeuroneParCouche.Count);
  SetLength(LActivations, FListeNeuroneParCouche.Count);

  LActivation := TDoubleMatrix.Create(AData.X, 1, 1);

  for LI := 0 to (FListeNeuroneParCouche.Count - 1) do
  begin
    LZ := FCoefficients[LI].ElementWiseMult(LActivation);
    LZ.AddInPlace(FBiais[LI]);
    LZs[LI] := LZ;
    LActivation := Sigmoid(LZ);
    LActivations[LI] := Sigmoid(LZ);
  end;
  LDelta := cost_derivate(LActivations[Length(LActivations) - 1], TDoubleMatrix.Create(AData.Y, 1, 1))
            .Mult(LZs[Length(LZs) - 1]);
  ADeltaNablaBiais[Length(ADeltaNablaBiais)-1] := LDelta;
  ADeltaNablaCoeff[Length(ADeltaNablaCoeff)-1] := LDelta.ElementWiseMult(LActivations[Length(LActivations) - 2].Transpose);

  for LI := (Length(LActivations) - 2) DOWNTO 1 do
  begin
    LSP := Sigmoid_Prime(LZs[LI]);
    LCoeffiecientsTrans := FCoefficients[LI + 1].Transpose;
    LDelta := LCoeffiecientsTrans.ElementWiseMult(LDelta);
    LDelta.MultInPlace(LSP);
    ADeltaNablaBiais[LI] := LDelta;
    ADeltaNablaCoeff[LI] := LDelta.ElementWiseMult(LActivations[LI-1].Transpose);
  end;

end;

function TNeuralNetwork.cost_derivate(AResultatActivation,
  AY: TDoubleMatrix): TDoubleMatrix;
begin
  Result := AResultatActivation.Sub(AY);
end;

procedure TNeuralNetwork.feedforward(var A: TDoubleMatrix);
VAR
  LI : Integer;
  LW , LB,
  LWA, LWAB : TDoubleMatrix;
begin
  for LI := 0 to (FListeNeuroneParCouche.Count - 1) do
  begin
    LW := FCoefficients [LI];
    LB := FBiais[LI];
    LWA  := LW.Mult(A);
    LWAB := LWA.Add(LB);
    A    := Sigmoid(LWAB);
  end;
end;

function TNeuralNetwork.fragmenterTList(AList      : TList<TCoordDouble>;
                                        ATailleLot : Integer): TList<TList<TCoordDouble>>;
var
  LK : Integer;
  LListeFragmentee : TList<TCoordDouble>;
begin
  Result := TList<TList<TCoordDouble>>.Create;
  for LK := 0 to (AList.Count - 1) do
  BEGIN
    IF ((LK DIV ATailleLot) = 0) then
    Begin
     if (LK <> 0) then Result.Add(LListeFragmentee);

     LListeFragmentee := TList<TCoordDouble>.Create;
     LListeFragmentee.Add(AList[LK]);
    end
    else LListeFragmentee.Add(AList[LK]);
  END;
  Result.Add(LListeFragmentee);
end;

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
    FCoefficients[LI] := TDoubleMatrix.CreateRand(FListeNeuroneParCouche[LJ],
                                           FListeNeuroneParCouche[LI], raSystem, 0);
  end;



end;
procedure TNeuralNetwork.libererTListTList(
  AListList: TList<TList<TCoordDouble>>);
VAR
  LI     : Integer;
  LListe : TList<TCoordDouble>;
begin
  for LI := 0 to (AListList.Count - 1) do
  begin
    LListe := AListList[LI];
    FreeAndNil(LListe);
  end;
  FreeAndNil(AListList);
end;

PROCEDURE TNeuralNetwork.remplirInitialiserTabMatrix(ATabMatrixARemplir,
                                       ATabMatrixRef     : TArray<TDoubleMatrix>);
var LI : Integer;
BEGIN
  SetLength(ATabMatrixARemplir, Length(ATabMatrixRef));
  for LI := 0 to (Length(ATabMatrixARemplir) - 1) do
    ATabMatrixARemplir[LI] := TDoubleMatrix.Create(ATabMatrixRef[LI].Width, ATabMatrixRef[LI].Height);
END;

procedure TNeuralNetwork.mettreAJourLotDeData(ALotData: TList<TCoordDouble>;
                                              AEtat: Integer);

var
  LNablas_Biais,
  LNablas_Coefficients : TArray<TDoubleMatrix>;
  LJ : Integer;
begin
  remplirInitialiserTabMatrix(LNablas_Coefficients, FCoefficients);
  remplirInitialiserTabMatrix(LNablas_Biais       , FBiais);
  for LJ := 0 to (ALotData.Count - 1) do
  begin
    BackPropagation(ALotData[LJ], LNablas_Coefficients, LNablas_Biais);
  end;
end;

procedure TNeuralNetwork.prendreExponentiel(var Value: double);
begin
  Value := exp(Value);
end;

PROCEDURE TNeuralNetwork.prendreInverse(var Value: double);
begin
  Value := 1 / Value;
end;
procedure TNeuralNetwork.shuffle(var AListeAMelanger: TList<TCoordDouble>);
var LI : Integer;
begin
  for LI := (AListeAMelanger.Count - 1) Downto 1 do
    AListeAMelanger.Exchange(LI, Random(LI+1));
end;

function TNeuralNetwork.Sigmoid(Z: TDoubleMatrix): TDoubleMatrix;
begin
  result := z.ElementwiseFunc(prendreExponentiel);
  result := result.add(1);
  result := result.ElementwiseFunc(prendreInverse);
end;

function TNeuralNetwork.Sigmoid_Prime(Z: TDoubleMatrix): TDoubleMatrix;
var LMatrixUnite : TDoubleMatrix;
begin
  LMatrixUnite.Create(Z.Width, Z.Height, 1);
  Result := Sigmoid(Z).Mult(LMatrixUnite.Sub(Sigmoid(Z)));
end;

procedure TNeuralNetwork.StochasticGradientDescent(
                                         ATrainingData      : TList<TCoordDouble>;
                                         AEta, ANbPass,
                                         ATailleDuMiniBatch : Integer;
                                         ATestData          : TList<TCoordDouble>);
var
  LJ, LK,
  LCptTestData,
  LCptTrainingData : Integer;
  LLotsTrainingData : TList<TList<TCoordDouble>>;
begin
  if Assigned(ATestData) then LCptTestData := ATestData.Count;

  LCptTrainingData := ATrainingData.Count;

  for LJ := 0 to (ANbPass -1) do
  Begin
    shuffle(ATrainingData);
    LLotsTrainingData := fragmenterTList(ATrainingData, ATailleDuMiniBatch);

    for LK := 0 to (LLotsTrainingData.Count - 1) do
    BEGIN

    END;


    libererTListTList(LLotsTrainingData);
  End;
end;

end.
