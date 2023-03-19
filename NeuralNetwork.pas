unit NeuralNetwork;
// L'algorithme ci-dessous est une traduction d'un algorithme de type reseau neuronaux
// écrit en Python par Michael Nielsen sur http://neuralnetworksanddeeplearning.com/chap1.html
// Cette traduction utilise la librairie de matrix de Mike Rabat https://github.com/mikerabat/mrmath
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
    FBiais                 : TArray<IMatrix>;
    FCoefficients          : TArray<IMatrix>;

    constructor Init(CONST AListeNeuroneParCouche : TList<Integer>);
    procedure StochasticGradientDescent(ATrainingData      : TList<TCoordDoubleMatrix>;
                                        ANbPass, //EPoch en anglais
                                        ATailleDuMiniBatch,
                                        AEta      : Integer;    // Param�tre
                                        ATestData : TList<TCoordDoubleMatrix> = NIL);
    FUNCTION Sigmoid(Z : IMatrix) : IMatrix;
    FUNCTION Sigmoid_Prime(Z: IMatrix): IMatrix;
  PUBLIC
    PROCEDURE MatrixTextToFile (AMatrix : IMatrix);
    procedure feedforward(VAR A : IMatrix);
    function RenvoyerIndiceMax(AMatrix: IMatrix): Integer;
  private
    FListeNeuroneParCouche : TList<Integer>;
    Function fragmenterTList(AList : TList<TCoordDoubleMatrix>; ATailleLot : Integer) : TList<TList<TCoordDoubleMatrix>>;
    procedure libererTListTList(AListList : TList<TList<TCoordDoubleMatrix>>);
    procedure shuffle(VAR AListeAMelanger : TList<TCoordDoubleMatrix>);
    FUNCTION  evaluate(ATestData : TList<TCoordDoubleMatrix>) : Integer;
    procedure prendreExponentiel(var Value: double);
    procedure prendreRandomValue(var Value: double);
    procedure prendreInverse(var Value: double);
    procedure decompterIntfCount(CONST AArray: TArray<IMatrix>);
    procedure mettreAJourLotDeData (ALotData : TList<TCoordDoubleMatrix>;
                                    AEtat    : Integer);
    procedure BackPropagation(CONST AData : TCoordDoubleMatrix;
                              out ADeltaNablaCoeff, ADeltaNablaBiais : TArray<IMatrix>);
    procedure remplirInitialiserTabMatrix(VAR ATabMatrixARemplir : TArray<IMatrix>;
                                          CONST ATabMatrixRef    : TArray<IMatrix>);
    Function cost_derivate(CONST AResultatActivation : IMatrix; AY : IMatrix): IMatrix;




  end;
implementation
USes
  System.SysUtils,
  MtxUtilFunc,
  RandomEng;
{ TNeuralNetwork }

procedure TNeuralNetwork.BackPropagation(const AData                  : TCoordDoubleMatrix;
                                         out ADeltaNablaCoeff, ADeltaNablaBiais : TArray<IMatrix>);
var
  LI : Integer;
  LZ,
  LSP,
  LDelta,
  LActivation,
  LActivationTrans,
  LCoeffiecientsTrans : IMatrix;
  LZs,
  LActivations : TArray<IMatrix>;
begin
  remplirInitialiserTabMatrix(ADeltaNablaCoeff, FCoefficients);
  remplirInitialiserTabMatrix(ADeltaNablaBiais, FBiais);
  SetLength(LZs, FListeNeuroneParCouche.Count);
  SetLength(LActivations, FListeNeuroneParCouche.Count);

  LActivation := AData.X.Clone;
  LActivations[0] := LActivation.Clone;
  for LI := 1 to (FListeNeuroneParCouche.Count - 1) do
  begin
    LZ := FCoefficients[LI].Mult(LActivation);
    LZ.AddInPlace(FBiais[LI]);
    LZs[LI] := LZ;
    LActivation := Sigmoid(LZ);
    LActivations[LI] := LActivation.Clone;
  end;

  LDelta := cost_derivate(LActivations[Length(LActivations) - 1], AData.Y);
  LSP := Sigmoid_Prime(LZs[Length(LZs) - 1]);
  LDelta.ElementWiseMultInplace(LSP);
  ADeltaNablaBiais[Length(ADeltaNablaBiais)-1] := LDelta;
  LActivationTrans := LActivations[Length(LActivations) - 2].Transpose;
  ADeltaNablaCoeff[Length(ADeltaNablaCoeff)-1] := LDelta.Mult(LActivationTrans);

  for LI := (FListeNeuroneParCouche.Count - 2) DOWNTO 1 do
  begin
    LSP := Sigmoid_Prime(LZs[LI]);
    LCoeffiecientsTrans := FCoefficients[LI + 1].Transpose;
    LDelta              := LCoeffiecientsTrans.mult(LDelta);
    LDelta.ElementWiseMultInPlace(LSP);
    ADeltaNablaBiais[LI] := LDelta;
    LActivationTrans     := LActivations[LI-1].Transpose;
    ADeltaNablaCoeff[LI] := LDelta.Mult(LActivationTrans);
  end;
  LCoeffiecientsTrans := NIL;
  LActivationTrans := NIL;
  LActivation := NIL;
  LDelta := NIL;
  LSP := NIL;
  LZ := NIL;
  decompterIntfCount(LZs);
  decompterIntfCount(LActivations);
end;

function TNeuralNetwork.cost_derivate(CONST AResultatActivation : IMatrix; AY : IMatrix): IMatrix;
begin
  Result := AResultatActivation.Sub(AY);
end;

procedure TNeuralNetwork.decompterIntfCount(const AArray: TArray<IMatrix>);
VAR LI : Integer;
begin
  FOR LI := 0 TO (Length(AArray)-1) DO
  BEGIN
    AArray[LI] := NIL;
  END;
end;
FUNCTION TNeuralNetwork.RenvoyerIndiceMax(AMatrix : IMatrix) : Integer;
  VAR
    LDoubleMax : Double;
    LI : Integer;
  Begin
    Result := 0;
    LDoubleMax := 0;
    FOR LI := 0 TO (AMatrix.Height - 1) DO
    BEGIN
      IF AMatrix.Items[0, LI] > LDoubleMax THEN
      BEGIN
        LDoubleMax := AMatrix.Items[0, LI];
        Result := LI;
      END;
    END;
  End;
FUNCTION TNeuralNetwork.evaluate(ATestData: TList<TCoordDoubleMatrix>) : Integer;

VAR
  LMatriceDeSortie : IMatrix;
  LValeurCalcule : Double;
  LJ : Integer;
begin
  Result := 0;
  For LJ := 0 TO (ATestData.Count - 1) DO
  BEGIN
    LMatriceDeSortie := ATestData[LJ].X.Clone;
    feedforward(LMatriceDeSortie);
    LValeurCalcule := RenvoyerIndiceMax(LMatriceDeSortie);
    IF (LValeurCalcule = ATestData[LJ].YDouble) THEN Inc(Result);
  END;
end;

procedure TNeuralNetwork.feedforward(var A: IMatrix);
VAR
  LI : Integer;
  LW , LB,
  LWA, LWAB : IMatrix;
begin
  for LI := 1 to (FListeNeuroneParCouche.Count - 1) do
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
  LK,
  LCompteur : Integer;
  LListeFragmentee : TList<TCoordDoubleMatrix>;
begin
  Result := TList<TList<TCoordDoubleMatrix>>.Create;
  LListeFragmentee := TList<TCoordDoubleMatrix>.Create;
  LCompteur := 0;
  for LK := 0 to (AList.Count - 1) do
  BEGIN
    IF ((LCompteur MOD ATailleLot) = 0) then
    Begin
      IF (LCompteur <> 0) THEN
      BEGIN
        LListeFragmentee.Add(AList[LK]);
        Result.Add(LListeFragmentee);
        LListeFragmentee := TList<TCoordDoubleMatrix>.Create;
      END
      ELSE LListeFragmentee.Add(AList[LK]);
    end
    else LListeFragmentee.Add(AList[LK]);
    Inc(LCompteur);
  END;
  IF NOT(Result.Contains(LListeFragmentee)) THEN
    IF LListeFragmentee.Count > 0 THEN
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

  setLength(FCoefficients, FListeNeuroneParCouche.Count);
  setLength(FBiais       , FListeNeuroneParCouche.Count);

  for LI := 1 to (FListeNeuroneParCouche.Count - 1) do
  begin
    FBiais[LI] := TDoubleMatrix.Create(1, FListeNeuroneParCouche[LI], 1);
    FBiais[LI].ElementwiseFuncInPlace(prendreRandomValue);
  end;

  for LI := 1 to (FListeNeuroneParCouche.Count - 1) do
  begin
    LJ := LI - 1;
    FCoefficients[LI] := TDoubleMatrix.Create(FListeNeuroneParCouche[LJ],
                                              FListeNeuroneParCouche[LI], 1);
    FCoefficients[LI].ElementwiseFuncInPlace(prendreRandomValue);
  end;
end;

procedure TNeuralNetwork.libererTListTList(
  AListList: TList<TList<TCoordDoubleMatrix>>);
VAR
  LI, LJ : Integer;
  LListe : TList<TCoordDoubleMatrix>;
begin
  for LI := 0 to (AListList.Count - 1) do
  begin
    LListe := AListList[LI];
    FOR LJ := 0 TO (LListe.Count - 1) DO
      TCoordDoubleMatrix(LListe[LJ]).MisANilMatrix;

    FreeAndNil(LListe);
  end;
  FreeAndNil(AListList);
end;

procedure TNeuralNetwork.MatrixTextToFile(AMatrix: IMatrix);
begin
  MatrixToTxtFile('Voir',TDoubleMatrix(AMatrix));
end;

PROCEDURE TNeuralNetwork.remplirInitialiserTabMatrix(VAR ATabMatrixARemplir : TArray<IMatrix>;
                                                     CONST ATabMatrixRef    : TArray<IMatrix>);
var LI : Integer;
BEGIN
  SetLength(ATabMatrixARemplir, Length(ATabMatrixRef));
  for LI := 1 to (Length(ATabMatrixARemplir) - 1) do
    ATabMatrixARemplir[LI] := TDoubleMatrix.Create(ATabMatrixRef[LI].Width, ATabMatrixRef[LI].Height);
END;

procedure TNeuralNetwork.mettreAJourLotDeData(ALotData: TList<TCoordDoubleMatrix>;
                                              AEtat: Integer);

var
  LMatrixScaleCoef,
  LMatrixScaleBiais : IMatrix;
  LNablas_Biais,
  LNablas_Coefficients,
  LDeltaNablas_Biais,
  LDeltaNablas_Coefficients : TArray<IMatrix>;
  LJ, LK : Integer;
begin
  remplirInitialiserTabMatrix(LNablas_Coefficients, FCoefficients);
  remplirInitialiserTabMatrix(LNablas_Biais       , FBiais);
  for LJ := 0 to (ALotData.Count - 1) do
  begin
    BackPropagation(ALotData[LJ], LDeltaNablas_Coefficients, LDeltaNablas_Biais);
    for LK := 1 to (Length(LDeltaNablas_Coefficients) - 1) do
    begin
      LNablas_Coefficients[LK] := LNablas_Coefficients[LK].Add(LDeltaNablas_Coefficients[LK]);
      LNablas_Biais[LK]        := LNablas_Biais[LK].Add(LDeltaNablas_Biais[LK]);
    end;
  end;
  for LJ := 1 to (FNbCouches - 1) do
  begin
    LMatrixScaleCoef  := LNablas_Coefficients[LJ].Scale(AEtat/ALotData.Count);
    LMatrixScaleBiais := LNablas_Biais[LJ].Scale(AEtat/ALotData.Count);
    FCoefficients[LJ].SubInPlace(LMatrixScaleCoef);
    FBiais[LJ].SubInPlace(LMatrixScaleBiais);
  end;
  LMatrixScaleCoef  := NIL;
  LMatrixScaleBiais := NIL;
  decompterIntfCount(LNablas_Biais);
  decompterIntfCount(LNablas_Coefficients);
  decompterIntfCount(LDeltaNablas_Biais);
  decompterIntfCount(LDeltaNablas_Coefficients);
end;

procedure TNeuralNetwork.prendreExponentiel(var Value: double);
begin
  Value := exp(-Value);
end;

PROCEDURE TNeuralNetwork.prendreInverse(var Value: double);
begin
  Value := 1 / Value;
end;
procedure TNeuralNetwork.prendreRandomValue(var Value: double);
begin
  Value := Value * RandG(0.0, 1.0);
end;

procedure TNeuralNetwork.shuffle(var AListeAMelanger: TList<TCoordDoubleMatrix>);
var LI : Integer;
begin
  for LI := (AListeAMelanger.Count - 1) Downto 1 do
    AListeAMelanger.Exchange(LI, Random(LI+1));
end;

function TNeuralNetwork.Sigmoid(Z: IMatrix): IMatrix;
begin
  result := z.ElementwiseFunc(prendreExponentiel);
  result := result.add(1);
  result := result.ElementwiseFunc(prendreInverse);
end;

function TNeuralNetwork.Sigmoid_Prime(Z: IMatrix): IMatrix;
var
  LMatrixUnite : IMatrix;
  LMatrixSigmoid : IMatrix;
  LMatrixUniteSubBySigmoid : IMatrix;
begin
  LMatrixUnite := TDoubleMatrix.Create(Z.Width, Z.Height, 1);
  LMatrixSigmoid := Sigmoid(Z);
  LMatrixUniteSubBySigmoid := LMatrixUnite.Sub(LMatrixSigmoid);
  Result := LMatrixSigmoid.ElementWiseMult(LMatrixUniteSubBySigmoid);
end;

procedure TNeuralNetwork.StochasticGradientDescent(
                                         ATrainingData      : TList<TCoordDoubleMatrix>;
                                         ANbPass, //EPoch en anglais
                                         ATailleDuMiniBatch,
                                         AEta      : Integer;    // Param�tre
                                         ATestData          : TList<TCoordDoubleMatrix>);
var
  LChaineDebug : STRING;
  LJ, LK : Integer;
  LLotsTrainingData : TList<TList<TCoordDoubleMatrix>>;
begin
  for LJ := 0 to (ANbPass -1) do
  Begin
    shuffle(ATrainingData);
    LLotsTrainingData := fragmenterTList(ATrainingData, ATailleDuMiniBatch);

    for LK := 0 to (LLotsTrainingData.Count - 1) do
      mettreAJourLotDeData(LLotsTrainingData[LK], AEta);


    IF Assigned(ATestData)
    THEN LChaineDebug := 'Pass N° ' + IntToStr(LJ)    + ' : '
                      + IntToStr(evaluate(ATestData)) + ' / '
                      + IntToStr(ATestData.Count)
    ELSE LChaineDebug := 'Pass N° ' + IntToStr(LJ) + ' terminé';
    WriteLn(LChaineDebug);

    libererTListTList(LLotsTrainingData);
  End;
end;

end.
