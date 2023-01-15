program ReseauNN;

uses
  Vcl.Forms,
  LearningNN in 'LearningNN.pas' {Form1},
  NeuralNetwork in 'NeuralNetwork.pas',
  CoordDouble in 'CoordDouble.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
