import Data.Bits (xor)
import Data.Char (chr)
import System.IO
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Crypto.Cipher.AES as AES


-- Exercise 9

pad_length :: Int -> Int -> Int
pad_length _ 0 = 0
pad_length block_length remainder_length = block_length - remainder_length 

pad :: C8.ByteString -> Int -> C8.ByteString
pad input block_length = C8.append input padding
    where remainder_length = mod (C8.length input) block_length
          pad_char = chr 4
          padding = C8.pack $ replicate (pad_length block_length remainder_length) pad_char

-- Exercise 10

-- copied
bytestring_xor :: C8.ByteString -> C8.ByteString -> C8.ByteString
bytestring_xor a b = BS.pack $ zipWith xor (BS.unpack a) (BS.unpack b)

decrypt_block :: AES.AES -> C8.ByteString -> C8.ByteString -> C8.ByteString
decrypt_block key block_ciphertext previous_block_ciphertext = aes_decrypted `bytestring_xor` previous_block_ciphertext
    where aes_decrypted = AES.decryptECB key block_ciphertext

encrypt_block :: AES.AES -> C8.ByteString -> C8.ByteString -> C8.ByteString
encrypt_block key block_plaintext previous_block_ciphertext = AES.encryptECB key prepared_block_plaintext
    where prepared_block_plaintext = block_plaintext `bytestring_xor` previous_block_ciphertext

split_by_block_size :: Int -> BS.ByteString -> [BS.ByteString]
split_by_block_size block_size bs
    | C8.length bs < block_size = []
    | otherwise = prefix : split_by_block_size block_size remainder
        where (prefix, remainder) = BS.splitAt block_size bs    

decryptCBC :: AES.AES -> C8.ByteString -> C8.ByteString -> C8.ByteString
decryptCBC key iv ciphertext = BS.concat zip_result
    where split_ciphertext = split_by_block_size 16 ciphertext
          ciphertext_with_iv = iv : split_ciphertext
          zip_result = zipWith (decrypt_block key) split_ciphertext ciphertext_with_iv

encryptCBC :: AES.AES -> C8.ByteString -> C8.ByteString -> C8.ByteString
encryptCBC key iv plaintext = BS.concat $ encrypt_blocks key iv split_plaintext
    where split_plaintext = split_by_block_size 16 $ pad plaintext 16

encrypt_blocks :: AES.AES -> C8.ByteString -> [C8.ByteString] -> [C8.ByteString]
encrypt_blocks _ _ [] = []
encrypt_blocks key iv (x : plaintext_blocks) = encrypted_block : encrypt_blocks key encrypted_block plaintext_blocks 
    where encrypted_block = encrypt_block key iv x  

ex10 = do
    withFile "data/10.txt" ReadMode (\handle -> do
        contents <- C8.hGetContents handle
        case B64.decode contents of
            Right input -> do
                let key = AES.initAES (C8.pack "YELLOW SUBMARINE")
                let iv = C8.pack $ replicate 16 $ chr 0
                let decrypted = decryptCBC key iv input
                print decrypted
            Left err -> print err
        )          