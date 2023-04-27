module Main where

import GUI.GUI as GUI

main :: IO ()
main = do
  GUI.write "./gui/pintrgui" (intrGUI (1280, 720))
  GUI.write "./gui/pmaingui" (mainGUI (1280, 720))
  GUI.write "./gui/poptsgui" (optsGUI (1280, 720))
  GUI.write "./gui/pinfogui" (infoGUI (1280, 720))
  GUI.write "./gui/defgui"   (defaultGUI (1280, 720))
