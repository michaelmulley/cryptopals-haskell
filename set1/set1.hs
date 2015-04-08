import Data.Bits
import Data.Char
import Data.List (maximumBy, minimumBy, nub)
import Data.Ord (comparing)
import System.IO 
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.ByteString.Char8 as C8Strict
import qualified Data.Map.Lazy as Map

import qualified Data.ByteString.Base16.Lazy as B16
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Crypto.Cipher.AES as AES


-- Exercise 1: hex/b64 encoding
-- for future exercises, I used a library to do this

hex2int :: [Char] -> Integer
hex2int inp = foldl (\i hc -> xor (shiftL i 4) (toInteger (digitToInt hc))) 0 inp

b64alphabet :: Int -> Char
b64alphabet a
    | a < 26 = chr (ord 'A' + a)
    | a < 52 = chr (ord 'a' + (a - 26))
    | a < 62 = chr (ord '0' + (a - 52))
    | a == 62 = '+'
    | a == 63 = '/'

int2b64_ :: Integer -> [Char]
int2b64_ 0 = []
int2b64_ a = b64alphabet (fromInteger (a .&. 63)) : int2b64_ (shiftR a 6)

int2b64 a = reverse $ int2b64_ a

-- Exercise 2

bytestring_xor a b = BS.pack $ zipWith xor (BS.unpack a) (BS.unpack b)
read_hex st = fst $ B16.decode $ C8.pack st
hex_xor a b = C8.unpack $ B16.encode $ bytestring_xor (read_hex a) (read_hex b)

-- Exercise 3

xor_with_repeating_key :: BS.ByteString -> BS.ByteString -> BS.ByteString
xor_with_repeating_key a key = bytestring_xor a (BS.cycle key)

countChars str = [(chr, C8.count chr lowstr) | chr <- ' ' : ['a'..'z']]
    where lowstr = C8.map toLower str

charFrequencies str = [(chr, (fromIntegral cnt) / len) | (chr, cnt) <- counts]
    where counts = countChars str
          -- len = fromIntegral $ sum $ map snd counts
          len = fromIntegral $ C8.length str

freqDifferenceScore freqa freqb = sum [ abs (a - b) | ((_, a), (_, b)) <- zip freqa freqb]

englishScore str = diffscore
    where freq = charFrequencies str
          -- These are the frequencies from Austen's "Emma"
          englishFreq = [(' ',0.16642749002052823),('a',6.130174473069236e-2),('b',1.2548037304792965e-2),('c',1.7430397341362754e-2),('d',3.2220645247111e-2),('e',9.697194474850378e-2),('f',1.7067404976602776e-2),('g',1.5412520531051065e-2),('h',4.798713969907707e-2),('i',5.25391992298249e-2),('j',1.2637094437762029e-3),('k',5.369356004198086e-3),('l',3.119705187070708e-2),('m',2.3340859976258947e-2),('n',5.330802156760846e-2),('o',5.997152426355951e-2),('p',1.1882927071226542e-2),('q',1.0258479873651602e-3),('r',4.612257643412985e-2),('s',4.7919501370239814e-2),('t',6.673761175824708e-2),('u',2.3274348952902306e-2),('v',8.733235558371313e-3),('w',1.836380627931699e-2),('x',1.553426952295814e-3),('y',1.7705459878634294e-2),('z',1.9727845910868464e-4)]
          diffscore = freqDifferenceScore freq englishFreq

-- returns a list of 256 (char, bytestring) tuples: xor against each repeating strings of each single byte
xor_against_single_chars :: BS.ByteString -> [(BS.ByteString, BS.ByteString)]
xor_against_single_chars input = [(key, xor_with_repeating_key input key) | key <- keys]
    where keys = [C8.singleton (chr key) | key <- [0..255]]

break_single_xor bs = minimumBy (comparing snd) xors_with_scores
    where xors_with_scores = [(plaintext, englishScore (snd plaintext)) | plaintext <- xor_against_single_chars bs]
ex3 = break_single_xor $ read_hex "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

-- Exercise 4

find_single_xor inputs = minimumBy (comparing snd) (map break_single_xor inputs)
ex4 = do
    withFile "data/4.txt" ReadMode (\handle -> do
        contents <- C8.hGetContents handle
        let inputs = map (fst . B16.decode)  (C8.lines contents)
        putStr $ show $ find_single_xor inputs)

-- Exercise 5

ex5 = B16.encode $ xor_with_repeating_key plaintext key
    where plaintext = (C8.pack "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal")
          key = (C8.pack "ICE")

-- Exercise 6

hamming_dist :: BS.ByteString -> BS.ByteString -> Int
hamming_dist a b = sum $ map popCount $ zipWith xor (BS.unpack a) (BS.unpack b)

test_hamming_dist = hamming_dist (C8.pack "this is a test") (C8.pack "wokka wokka!!!")

split_by_keysize keysize bs
    | C8.length bs < keysize = []
    | otherwise = prefix : split_by_keysize keysize remainder
        where (prefix, remainder) = BS.splitAt keysize bs

normalized_edit_distance_for_keysize keysize bs = (sum normalized_hamming_distances) / fromIntegral (length normalized_hamming_distances)
    where chunks = split_by_keysize keysize bs
          hamming_distances = zipWith hamming_dist chunks (tail chunks)
          normalized_hamming_distances = [ (fromIntegral hd) / (fromIntegral keysize) | hd <- hamming_distances]

best_keysize bs = fst $ minimumBy (comparing snd) edit_distances
    where edit_distances = [(keysize, normalized_edit_distance_for_keysize keysize bs) | keysize <- [2..40]]

break_repeating_xor bs = broken_key
    where keysize = best_keysize bs
          keysize_blocks = split_by_keysize keysize bs
          blocks_with_single_char_key = [ BS.pack $ map (\block -> BS.index block index) keysize_blocks | index <- [0..keysize - 1]]
          broken_single = map break_single_xor blocks_with_single_char_key
          broken_key = C8.pack $ map (C8.head . fst . fst) broken_single

ex6 = do
    withFile "data/6.txt" ReadMode (\handle -> do
        contents <- C8.hGetContents handle
        case B64.decode contents of
            Right input -> do
                let broken_key = break_repeating_xor input
                putStr $ show $ broken_key
                putStr $ show $ xor_with_repeating_key input broken_key
            Left err -> putStr err
        )

-- Exercise 7

ex7 = do
    withFile "data/7.txt" ReadMode (\handle -> do
        contents <- C8.hGetContents handle
        putStr $ show $ ex7' contents
        )

ex7' :: C8.ByteString -> Either String C8Strict.ByteString
ex7' contents = do
                  input <- B64.decode contents
                  let key = AES.initAES (C8Strict.pack "YELLOW SUBMARINE")
                  let decrypted = AES.decryptECB key (C8.toStrict input)
                  return decrypted

-- Exercise 8

ex8 = do
    withFile "data/8.txt" ReadMode (\handle -> do
        contents <- C8.hGetContents handle
        let inputs = map (fst . B16.decode)  (C8.lines contents)
        let split_inputs = map (split_by_keysize 16) inputs
        let culprit = head $ filter (\l -> length (nub l) < length l) split_inputs
        putStr $ show $ B16.encode $ C8.concat culprit)