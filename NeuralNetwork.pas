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
    FTrainingDate : TList<TCoordDoubleMatrix>;
    Function fragmenterTList(AList : TList<TCoordDoubleMatrix>; ATailleLot : Integer) : TList<TList<TCoordDoubleMatrix>>;
    procedure libererTListTList(AListList : TList<TList<TCoordDoubleMatrix>>);
    procedure shuffle(VAR AListeAMelanger : TList<TCoordDoubleMatrix>);
    procedure prendreExponentiel(var Value: double);
    procedure feedforward(VAR A : TDoubleMatrix);
    procedure prendreInverse(var Value: double);
    procedure StochasticGradientDescent(ATrainingData      : TList<TCoordDoubleMatrix>;
                                        AEta,    // Param�tre
                                        ANbPass, //EPoch en anglais
                                        ATailleDuMiniBatch : Integer;
                                        ATestData          : TList<TCoordDoubleMatrix> = NIL);
    procedure mettreAJourLotDeData (ALotData : TList<TCoordDoubleMatrix>;
                                    AEtat    : Integer);
    procedure BackPropagation(CONST AData : TCoordDoubleMatrix;
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

procedure TNeuralNetwork.BackPropagation(const AData                  : TCoordDoubleMatrix;
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
  remplirInitialiserTabMatrix(ADeltaNablaCoeff, FCoefficients);
  remplirInitialiserTabMatrix(ADeltaNablaBiais, FBiais);
  SetLength(LZs, FListeNeuroneParCouche.Count);
  SetLength(LActivations, FListeNeuroneParCouche.Count);

  LActivation := AData.X;

  for LI := 0 to (FListeNeuroneParCouche.Count - 1) do
  begin
    LZ := FCoefficients[LI].Mult(LActivation);
    LZ.AddInPlace(FBiais[LI]);
    LZs[LI] := LZ;
    LActivation := Sigmoid(LZ);
    LActivations[LI] := Sigmoid(LZ);
  end;
  LDelta := cost_derivate(LActivations[Length(LActivations) - 1], AData.Y)
            .ElementWiseMult(LZs[Length(LZs) - 1]);
  ADeltaNablaBiais[Length(ADeltaNablaBiais)-1] := LDelta;
  ADeltaNablaCoeff[Length(ADeltaNablaCoeff)-1] := LDelta.Mult(LActivations[Length(LActivations) - 2].Transpose);

  for LI := (Length(LActivations) - 2) DOWNTO 1 do
  begin
    LSP := Sigmoid_Prime(LZs[LI]);
    LCoeffiecientsTrans := FCoefficients[LI + 1].Transpose;
    LDelta := LCoeffiecientsTrans.ElementWiseMult(LDelta);
    LDelta.ElementWiseMultInPlace(LSP);
    ADeltaNablaBiais[LI] := LDelta;
    ADeltaNablaCoeff[LI] := LDelta.Mult(LActivations[LI-1].Transpose);
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

function TNeuralNetwork.fragmenterTList(AList      : TList<TCoordDoubleMatrix>;
                                        ATailleLot : Integer): TList<TList<TCoordDoubleMatrix>>;
var
  LK : Integer;
  LListeFragmentee : TList<TCoordDoubleMatrix>;
begin
  Result := TList<TList<TCoordDoubleMatrix>>.Create;
  for LK := 0 to (AList.Count - 1) do
  BEGIN
    IF ((LK DIV ATailleLot) = 0) then
    Begin
      LListeFragmentee := TList<TCoordDoubleMatrix>.Create;
      LListeFragmentee.Add(AList[LK]);
      if (LK <> 0) then Result.Add(LListeFragmentee);
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
  AListList: TList<TList<TCoordDoubleMatrix>>);
VAR
  LI     : Integer;
  LListe : TList<TCoordDoubleMatrix>;
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

procedure TNeuralNetwork.mettreAJourLotDeData(ALotData: TList<TCoordDoubleMatrix>;
                                              AEtat: Integer);

var
  LNablas_Biais,
  LNablas_Coefficients,
  LDeltaNablas_Biais,
  LDeltaNablas_Coefficients : TArray<TDoubleMatrix>;
  LJ, LK : Integer;
begin
  remplirInitialiserTabMatrix(LNablas_Coefficients, FCoefficients);
  remplirInitialiserTabMatrix(LNablas_Biais       , FBiais);
  for LJ := 0 to (ALotData.Count - 1) do
  begin
    BackPropagation(ALotData[LJ], LDeltaNablas_Coefficients, LDeltaNablas_Biais);
    for LK := 0 to (Length(LDeltaNablas_Coefficients) - 1) do
    begin
      LNablas_Coefficients[LK] := LNablas_Coefficients[LK].Add(LDeltaNablas_Coefficients[LK]);
      LNablas_Biais[LK]        := LNablas_Biais[LK].Add(LDeltaNablas_Biais[LK]);
    end;
  end;
  for LJ := 0 to (FNbCouches - 1) do
  begin
    FCoefficients[LJ] := FCoefficients[LJ].Sub(LNablas_Coefficients[LJ].Scale((AEtat/ALotData.Count)));
    FBiais[LJ] := FBiais[LJ].Sub(LNablas_Biais[LJ].Scale((AEtat/ALotData.Count)));
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
procedure TNeuralNetwork.shuffle(var AListeAMelanger: TList<TCoordDoubleMatrix>);
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
  LMatrixUnite := TDoubleMatrix.Create(Z.Width, Z.Height, 1);
  Result := Sigmoid(Z).Mult(LMatrixUnite.Sub(Sigmoid(Z)));
end;

procedure TNeuralNetwork.StochasticGradientDescent(
                                         ATrainingData      : TList<TCoordDoubleMatrix>;
                                         AEta, ANbPass,
                                         ATailleDuMiniBatch : Integer;
                                         ATestData          : TList<TCoordDoubleMatrix>);
var
  LJ, LK,
  LCptTestData,
  LCptTrainingData : Integer;
  LLotsTrainingData : TList<TList<TCoordDoubleMatrix>>;
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
