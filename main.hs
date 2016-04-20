-- Importação de Bibliotecas
import Graphics.UI.Gtk -- Biblioteca Gtk2Hs
import System.IO -- Biblioteca do sistema
import Data.Time -- Biblioteca de Data/Hora



main :: IO ()
main= do
  initGUI -- Inicia uma aplicação com Interface de Usuário através d GTK
  window <- windowNew --Cria uma nova janela
  set window [windowTitle := "Seleção de Sexo por InputBox", containerBorderWidth := 100] -- Seta propriedades de janela criada

  vb <- vBoxNew False 0 -- Cria uma nova Caixa de valor
  containerAdd window vb -- Adiciona a nova Caixa de Valor a janela principal

  hb <- hBoxNew False 0 -- Cria uma nova Caixa de Valor
  boxPackStart vb hb PackNatural 0 -- Adiciona a nova Caixa de Valor a janela principal


  bt2 <- labelNew Nothing -- Cria um label
  labelSetMarkup bt2 "<big>Entre com seu Sexo(Masculino/Feminino): </big>" -- Seta o label como HTM e adiciona suas propriedades
  boxPackStart hb bt2 PackNatural 5 -- Adiciona o label na janela principal

  txtfield <- entryNew --Cria uma Entry(TextBox)
  boxPackStart hb txtfield PackNatural 5 --Adiciona a Entry na janela principal
  button <- buttonNew -- Cria um botao
  set button[buttonLabel := "Resposta Anterior"] -- Seta propriedades do botao
  boxPackStart hb button PackNatural 0 -- Adiciona o Botao na janela principal

  txtstack <- statusbarNew -- Cria uma pilha de texto
  -- (Pilha para guardar de depois apresentar os resultados anteriores no label)
  boxPackStart vb txtstack PackNatural 0 -- Adiciona a pilha no contexto
  id <- statusbarGetContextId txtstack "Line" -- seta uma ID para cada posição na pilha

  widgetShowAll window -- Apresenta todas as janelas e objetos criados
  widgetSetSensitivity button False

  onEntryActivate txtfield (saveText txtfield button txtstack id) -- Salva o conteudo da TextBox na pilha
  onPressed button (statusbarPop txtstack id) -- Ao pressionar o botao, pega o resultado anterior da pilha
  onDestroy window mainQuit -- Ao destruir a janela principal, sai do MAIN
  mainGUI



saveText :: Entry -> Button -> Statusbar -> ContextId -> IO () -- Função que salva o texto e mostra a saída no label
saveText fld b stk id = do
    txt <- entryGetText fld -- pega o texto da entrada

    		--Condições para saida(resultado)
    let mesg | txt == "" = "Erro! A Input Box nao pode ficar em branco"
             | txt == " " = "Erro! A Input Box nao pode ficar em branco"
             | txt == "Feminino" || txt == "Masculino" || txt == "feminino" || txt == "masculino" = "Sexo Selecionado: " ++ txt
             | otherwise = "Erro! Entre com um sexo válido (Masculino/Feminino). Sua resposta: " ++ txt


    appendFile "saida.txt" ("Sua Entrada: " ++ "'" ++ txt ++ "'\n") --Add Saida no arquivo TXT 
    widgetSetSensitivity b True
    msgid <- statusbarPush stk id mesg --pega resultado anterior atraves da id na pilha
    currentTime <- fmap show getZonedTime -- seta Data/Hora atual
    appendFile "saida.txt" ("OutPut: " ++ "'" ++ mesg ++ "'"  ++ "\n") --Add Saida no arquivo TXT
    appendFile "saida.txt" ("Data/Hora: " ++ currentTime ++ "\n\n")--Add Saida no arquivo TXT
    return ()