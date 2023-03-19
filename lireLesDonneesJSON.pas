unit lireLesDonneesJSON;

interface
USES
  Matrix,
  CoordDouble,
  System.Classes,
  System.JSON.Readers,
  System.Generics.Collections;
TYPE
  Charger = CLASS

    CLASS PROCEDURE ChargerDonneesEnJson(OUT AListeMatrix : TList<TCoordDoubleMatrix>; ANomFichier : STRING);
    CLASS FUNCTION  TransformerChiffreEnVecteur(CONST AChiffre : Integer) : TDoubleMatrix;
  END;
implementation
USES
  System.JSON.Types,
  System.SysUtils;
{ Charger }

CLASS PROCEDURE Charger.ChargerDonneesEnJson(OUT AListeMatrix : TList<TCoordDoubleMatrix>; ANomFichier : STRING);
var
  Lfs : TFileStream;
  LAdresseFichier : STRING;
  LJsonTextReader : TJsonTextReader;
  LStreamReader   : TStreamReader;
  LTab            : TArray<Double>;
  LMatrixDouble : TCoordDoubleMatrix;
begin
  LAdresseFichier := '..\..\Donnees\%s.json';
  LAdresseFichier := Format(LAdresseFichier, [ANomFichier]);
  Lfs := TFileStream.Create(LAdresseFichier, fmOpenRead);
  LStreamReader := TStreamReader.Create(Lfs);
  LJsonTextReader := TJsonTextReader.Create(LStreamReader);

  LTab := [];
  WHILE LJsonTextReader.Read DO
  BEGIN
    IF LJsonTextReader.IsStartToken(LJsonTextReader.TokenType) THEN
    BEGIN
      CASE LJsonTextReader.TokenType OF
        TJsonToken.StartObject :
        BEGIN
          LJsonTextReader.Read;
          LMatrixDouble.YDouble := StrToInt(LJsonTextReader.Value.AsString);
          LMatrixDouble.Y       := Charger.TransformerChiffreEnVecteur(StrToInt(LJsonTextReader.Value.AsString));
        END;

      END;
    END
    ELSE IF LJsonTextReader.IsEndToken(LJsonTextReader.TokenType) THEN
    BEGIN
      CASE LJsonTextReader.TokenType OF
        TJsonToken.EndArray :
        BEGIN
          IF Length(LTab) > 0 THEN
          BEGIN
            LMatrixDouble.X := TDoubleMatrix.Create(LTab, 1, Length(LTab));
            AListeMatrix.Add(LMatrixDouble);
          END;
          LTab := [];
        END;
      END;
    END
    ELSE
    BEGIN
      IF LJsonTextReader.TokenType = TJsonToken.Float THEN LTab := LTab + [LJsonTextReader.Value.AsExtended];
    END;
  END;

  LStreamReader.Destroy;
  Lfs.Destroy;
end;

CLASS FUNCTION Charger.TransformerChiffreEnVecteur(CONST AChiffre: Integer): TDoubleMatrix;
begin
  Result := TDoubleMatrix.Create(1,10);
  Result.Items[0, AChiffre] := 1;
end;

end.
