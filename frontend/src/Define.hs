module Define where

type IsNiku = Bool
type IsKiko = Bool --Kikurage & Komatuna
type Count = Int
data Kosu = One | Two deriving stock (Eq,Show)
data RMen = Futo | Hoso deriving stock (Eq,Show)
data IsCook = Raw | Cooked
data Men = Men RMen MSize | YMen RMen MSize Count | NoMen | Dame deriving stock (Eq,Show)
data YSize = Masi | Futu deriving stock (Eq,Show)
data MSize = Half | Norm | Big deriving stock (Eq,Show)
data YasaiP = YasaiP YSize IsNiku IsKiko | NoYasai deriving stock (Eq,Show)  

data PTare = Noukou | Miso | SGoma | Rayu | Kara | Syoyu deriving stock (Eq,Show)
data Tcc = T10 | T20 | T30 | T40 | T50 | T70 | T80 deriving stock (Eq,Show)
data Tare = Tare PTare Tcc deriving stock (Eq,Show)

data Siru = Pai | Dasi | NoSiru deriving stock (Eq,Show)
                             -- Paitan or KatuoDasi(For Syoyu)

data Soup = Soup YasaiP [Tare] Siru | PSoup YasaiP [Tare] Siru Count
                                                       deriving stock (Eq,Show)
data Don = DSiro | DKuro | DAo deriving stock Show
data Top = Naruto Kosu | Mizuna | Negi | Menma | Mituba | Yuzu 
         | Azitama | Tyasyu | Hikiniku | Itotou deriving stock (Eq,Show)

data Ramen = Ramen Men FGuzai [Top] deriving stock Show

data Zikan = T1_30 | T2_00 | T3_00 | T3_30 | T7_00
data Dousa = Yuderu Zikan | Ageru Zikan | Ireru | Futto | Niru | Yaku 

data Youki = Donburi Don Ramen | Tenabe Guzai | Sippai deriving stock Show

data Guzai = Guzai YasaiP [Tare] Siru deriving stock Show

data FGuzai = Fsp Soup | NoFutto | NoGuzai deriving stock (Eq,Show)

data Gu = Gz Guzai | Fgz FGuzai | Rmn Ramen 
        | Mn Men | Tp Top | Yp YasaiP | Tr Tare | Si Siru

yuderu :: RMen -> MSize -> Zikan -> Men
yuderu Futo ms T3_30 = YMen Futo ms 0
yuderu Hoso ms T1_30 = YMen Hoso ms 0
yuderu _ _ _ = Dame

ireru :: Youki -> Gu -> Youki
ireru (Donburi dn (Ramen mn fg tp)) (Mn nmn) 
  = if mn==NoMen && fg==NoGuzai && null tp then Donburi dn (Ramen nmn fg tp) 
                                             else Sippai
ireru (Donburi dn (Ramen mn fg tp)) (Fgz nfg) 
  = if mn/=NoMen && fg==NoGuzai && null tp then Donburi dn (Ramen mn nfg tp)  
                                             else Sippai
ireru (Donburi dn (Ramen mn fg tps)) (Tp tp) 
  = if mn/=NoMen && fg/=NoGuzai then Donburi dn (Ramen mn fg (tps<>[tp]))  
                                             else Sippai
ireru (Tenabe (Guzai yp trs sr)) (Yp nyp) 
  = if yp/=NoYasai then Tenabe (Guzai nyp trs sr) else Sippai
ireru (Tenabe (Guzai yp trs sr)) (Tr ntr) = Tenabe (Guzai yp (trs<>[ntr]) sr)
ireru (Tenabe (Guzai yp trs sr)) (Si nsr) 
  = if sr/=NoSiru then Tenabe (Guzai yp trs nsr) else Sippai
ireru _ _ = Sippai

futto :: Youki -> FGuzai
futto (Tenabe (Guzai yp tr sr)) = Fsp (PSoup yp tr sr 0)  
futto _ = NoFutto

noukouTanmen :: Youki
noukouTanmen = Donburi DSiro (Ramen (Men Futo Norm) (Fsp (Soup (YasaiP Futu True True) [Tare Noukou T70] Pai)) [Naruto Two,Mizuna])  

