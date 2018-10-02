* Encoding: UTF-8.

DATASET ACTIVATE DataSet3.
GLM stick testicle eye witchetty
  /WSFACTOR=Animal 4 Polynomial 
  /WSDESIGN=Animal
/MMATRIX =
    'Live vs. dead' stick 0.5 witchetty 0.5 testicle -0.5 eye -0.5;
    'Stick vs. witchetty'  stick 1 witchetty -1 testicle 0 eye 0;
    'Testicle vs. eye'  stick 0 witchetty 0 testicle 1 eye -1.
