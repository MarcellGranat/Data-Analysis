# A termékenységi ráta kapcsolata az egy főre eső bruttó kibocsátással és a munkanélküliséggel <img src="logo.png" align="right" width="140" height="140" />

Ez a repository az Új jövőkép MNB Tanulmányi Verseny *Új Demográfiai Program* szekciójába készült *A termékenységi ráta kapcsolata az egy főre eső bruttó kibocsátással és a munkanélküliséggel* című cikk kódjait tartalmazza.

A repository tartalma az alábbi:

### Felhasznált adatok: ujdemografiaiprogram.Rdata

A fájl az alábbi adatokat tartalmazza:

- `NeighbourCountry`: Általam szomszédosnak ítélt országok mátrixa
- `CountryData`: Országok elhelyezkedésének adatai ábra készítéséhez
- `socioeconomic_indicators`: Magyarorzság hosszú éves idősoros adatai (forrás: KSH/oecd)
- `c.panel`: Magyarország TTA, GDP/fő és munkanélküliségi ráta adatai megyénként (forrás: KSH)
- `oecd_fertility`: Országok TTA adatai (forrás: OECD)
- `hunsf`: Magyarország megyéinek shp fájlja ábra készítéséhez
- `LiveBirthAndFertility`: Magyarország születési indikátorai (forrás: KSH)

### ujdemografiaiprogram.md

A tanulmány kódjai outputokkal megjelenítve (**olvasáshoz ajánlott**).

### ujdemografiaiprogram.rmd

A tanulmány kódjai R-ben való futtatáshoz előkészítve.

### ujdemografiaiprogram_files

A tanulmányban megjelenő ábrák svg formátumban (felbontás eltérő).
