module TenSpec where

import Test.Hspec
import Ten

main = hspec $ do
  describe "Exercício 1" $ do
    it "retorna o último elemento da lista" $ 
      last' [20,19..1] `shouldBe` 1
    
    it "retorna error na lista vazio" $
      last' [] `shouldThrow` anyException
  
  describe "Exercício 2" $ do
    it "retorna o penúltimo elemento da lista" $
      lastOne [20, 19..1] `shouldBe` 2

    it "retorna error na lista vazia" $
      lastOne [] `shouldThrow` anyException
  
  describe "Exercício 3" $ do
    it "retorna o número em determinada posição" $
      element_at [0..10] 2 `shouldBe` 2 

    it "retornar error na lista vazia" $
      element_at [] 2 `shouldThrow` anyException
  
  describe "Exercicio 4" $ do
    it "retorna o numero de elementos em uma lista" $
      myLength [[1..10], [10,9..1], [1..10]] `shouldBe` 3
  
  describe "Exercicio 5" $ do
    it "retorna a lista ao contrario" $
      myReverse "A man, a plan, a canal, panama!" `shouldBe` "!amanap ,lanac a ,nalp a ,nam A"
  
  describe "Exercicio 6" $ do
    it "retorna se uma lista é palindromo" $
      isPalindrome "madamimadam" `shouldBe` True