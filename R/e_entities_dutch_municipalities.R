#' Entities: Dutch Municipalities 
#'
#' This is an example entity list with hard-coded items that could indicate property crimes. 
#' 
#' @param return.type 
#' Use \code{return.type = "character"} to return a character vector (default); 
#' return.type = "data.table" returns a data.table} 
#'
#' @return Character vector or \code{data.table}.
#' @export 
#'
#' @examples 
entities_dutch_municipalities <- function(return.type = c("character","data.table")){ 

    return.type  <- match.arg(return.type)
    
    entities.raw <- '
        lemma
        Aa en Hunze
        Aalburg
        Aalsmeer
        Aalten
        Abcoude
        Achtkarspelen
        Alblasserdam
        Albrandswaard
        Alkmaar
        Almelo
        Almere
        Alphen aan den Rijn 
        Alphen-Chaam
        Ameland
        Amersfoort
        Amstelveen
        Amsterdam
        Andijk 
        Anna Paulowna
        Apeldoorn
        Appingedam
        Arnhem
        Assen
        Asten 
        Baarle-Nassau
        Baarle
        Baarn
        Barendrecht
        Barneveld
        Bedum
        Beek
        Beemster
        Beesel
        Bellingwedde
        Blijham
        Bergambacht 
        Berkenwoude
        Bergeijk
        Bergen (Limburg)
        Bergen (Noord-Holland) 
        Bergen op Zoom
        Berkelland
        Bernheze
        Bernisse
        Best 
        Batadorp
        Beuningen
        Beverwijk
        De Bilt
        het Bildt 
        Binnenmaas
        Bladel
        Blaricum
        Bloemendaal
        Boornsterhem
        Boarnsterhim
        Bodegraven
        Boekel
        Ten Boer
        Bolsward
        Borger-Odoorn
        Borne
        Borsele
        Boskoop
        Boxmeer
        Boxtel
        Breda
        Breukelen
        Brielle
        Bronckhorst
        Brummen
        Brunssum
        Bunnik
        Bunschoten
        Buren
        Beusichem
        Bussum
        Capelle aan den IJssel
        Capelle
        Castricum
        Coevorden
        Cranendonck
        Cromstrijen
        Cuijk
        Culemborg 
        Dantumadeel
        Dantumadiel
        Delft
        Delfzijl
        Deurne
        Deventer
        Diemen 
        Dinkelland
        Dirksland
        Doesburg
        Doetinchem 
        Dongen
        Dongeradeel
        Dordrecht 
        Drechterland
        Drimmelen
        Dronten
        Druten
        Duiven
        Echt-Susteren 
        Edam-Volendam
        Ede
        Eemnes
        Eemsmond
        Eersel
        Eijsden 
        Eindhoven
        Elburg
        Emmen
        Enkhuizen
        Enschede
        Epe       
        Ermelo
        Etten-Leur
        Ferwerderadeel
        Ferwerderadiel
        Franekeradeel     
        Gaasterland-Sloten
        Gaasterlân-Sleat
        Geertruidenberg
        Geldermalsen
        Geldrop        
        Geldrop-Mierlo
        Gemert-Bakel
        Gennep
        Giessenlanden
        Gilze en Rijen
        Goedereede   
        Goes
        Goirle
        Gorinchem
        Gouda
        Graafstroom
        Graft-De Rijp
        Grave 
        Den Haag
        \'s-Gravenhage
        Groesbeek
        Groningen
        Grootegast   
        Gulpen-Wittem
        Haaksbergen
        Haaren
        Haarlem         
        Haarlemmerliede en Spaarnwoude
        Haarlemmermeer 
        Halderberge
        Hardenberg
        Harderwijk
        Hierden
        Hardinxveld-Giessendam 
        Haren
        Harenkarspel
        Harlingen
        Hattem
        Heemskerk
        Heemstede
        Heerde  Heerenveen
        Heerhugowaard
        Heerlen
        Heeze-Leende
        Heiloo
        Helden 
        Den Helder
        Hellendoorn
        Hellevoetsluis
        Helmond
        Hendrik-Ido-Ambacht
        Hengelo
        \'s-Hertogenbosch
        Heumen
        Heusden
        Hillegom
        Hilvarenbeek 
        Hilversum
        Hof van Twente
        Hoogeveen
        Hoogezand-Sappemeer
        Hoorn 
        Horst aan de Maas
        Houten
        Huizen
        Hulst
        IJsselstein
        Kaag en Braassem 
        Kampen
        Kapelle
        Katwijk
        Kerkrade
        Kessel
        Koggenland 
        Kollumerland en Nieuwkruisland
        Korendijk
        Krimpen aan den IJssel
        Laarbeek 
        Landerd
        Landgraaf
        Landsmeer
        Langedijk
        Lansingerland
        Laren 
        Leek
        Leerdam
        Leeuwarden
        Leeuwarderadeel
        Leiden
        Leiderdorp
        Leidschendam-Voorburg
        Lelystad
        Lemsterland
        Leudal
        Leusden
        Liesveld 
        Lingewaal
        Lingewaard
        Lisse
        Lith
        Littenseradeel
        Littenseradiel
        Lochem 
        Loenen
        Loon op Zand
        Lopik
        Loppersum
        Losser
        Maarssen
        Maasbree 
        Maasdonk
        Maasdriel
        Maasgouw
        Maasbracht
        Maassluis
        Maastricht
        Margraten 
        Marum
        Medemblik
        Meerlo-Wanssum
        Meerssen
        Meijel
        Menaldumadeel
        Menaldum 
        Menterwolde
        Meppel
        Middelburg
        Middelharnis
        Midden-Delfland
        Midden-Drenthe 
        Mill en Sint Hubert
        Millingen aan de Rijn
        Moerdijk
        Montferland
        Montfoort    
        Mook en Middelaar
        Muiden
        Naarden
        Neder-Betuwe
        Nederlek
        Nederweert
        Neerijnen 
        Niedorp
        Nieuwegein
        Nieuwkoop
        Nieuw-Lekkerland
        Nijefurd
        Nijkerk
        Nijmegen 
        Noord-Beveland
        Noordenveld
        Noordoostpolder
        Noordwijk
        Noordwijkerhout 
        Nuenen Gerwen en Nederwetten
        Nunspeet
        Nuth
        Oegstgeest
        Oirschot
        Oisterwijk 
        Oldambt
        Oldebroek
        Oldenzaal
        Oldenzaal
        Olst-Wijhe
        Ommen
        Onderbanken 
        Oosterhout
        Oostflakkee
        Oost Gelre
        Ooststellingwerf
        Oosterwolde
        Oostzaan 
        Oostzaan
        Opmeer
        Opsterland
        Oss
        Oud-Beijerland
        Oude IJsselstreek 
        Ouder-Amstel
        Ouderkerk
        Oudewater
        Oudewater
        Overbetuwe
        Papendrecht 
        Pekela
        Purmerend
        Putten
        Pijnacker-Nootdorp
        Raalte
        Reeuwijk 
        Reimerswaal
        Renkum
        Renswoude
        Reusel-De Mierden
        Rheden
        Rhenen 
        Ridderkerk
        Rijnwaarden
        Rijnwoude
        Rijssen-Holten
        Rijswijk
        Roerdalen 
        Roermond
        De Ronde Venen
        Roosendaal
        Rotterdam
        Rozendaal
        Rucphen
        Schagen  
        Schermer
        Scherpenzeel
        Schiedam
        Schiermonnikoog
        Schijndel
        Schinnen
        Schoonhoven 
        Schouwen-Duiveland
        Sevenum
        Simpelveld
        Sint Anthonis
        Sint-Michielsgestel 
        Sint-Oedenrode
        Sittard-Geleen
        Scharsterland
        Skarsterlân
        Slochteren
        Sluis 
        Smallingerland
        Sneek
        Soest
        Someren
        Son en Breugel
        Spijkenisse
        Stadskanaal 
        Staphorst
        Stede Broec
        Steenbergen
        Steenwijkerland
        Stein
        Strijen
        Terneuzen 
        Terschelling
        Texel
        Teylingen
        Tholen
        Tiel
        Tilburg
        Tubbergen
        Twenterand 
        Tynaarlo
        Tietjerksteradeel
        Tytsjerksteradiel
        Ubbergen
        Uden
        Uitgeest
        Uithoorn 
        Urk
        Utrecht
        Utrechtse Heuvelrug
        Vaals
        Valkenburg aan de Geul
        Valkenswaard 
        Veendam
        Veenendaal
        Veere
        Veghel
        Veldhoven
        Velsen
        Venlo
        Venray
        Vianen 
        Vlaardingen
        Vlagtwedde
        Vlieland
        Vlissingen
        Vlist
        Voerendaal
        Voorschoten 
        Voorst
        Vught
        Waalre
        Waalwijk
        Waddinxveen
        Wageningen
        Wassenaar
        Waterland 
        Weert
        Weesp
        Werkendam
        Wervershoof
        Westland
        West Maas en Waal
        Westerveld 
        Westervoort
        Weststellingwerf
        Westvoorne
        Wierden
        Wieringen
        Wieringermeer 
        Wijchen
        Wijdemeren
        Wijk bij Duurstede
        Winsum
        Winterswijk
        Wonseradeel 
        Wûnseradiel
        Woensdrecht
        Woerden
        Harmelen
        De Wolden
        Wormerland
        Woudenberg 
        Woudrichem
        Wymbritseradeel
        Wymbritseradiel
        Zaanstad
        Zaltbommel
        Zandvoort 
        Zederik
        Zeevang
        Zeewolde
        Zeist
        Zevenaar
        Zijpe
        Zoetermeer
        Zoeterwoude 
        Zuidhorn
        Zuidplas
        Zundert
        Zutphen
        Zwartewaterland
        Zwijndrecht
        Zwolle    
    '
     
    Encoding(entities.raw) <- "utf-8"
    
    retval <- data.table::fread(input = entities.raw, 
        stringsAsFactors = F, 
        blank.lines.skip = T,
        header = T, 
        sep = ";"   
    )
       
    retval[, lemma := stringi::stri_trans_tolower(stringi::stri_trim(retval$lemma))]
     
    retval[, code := "duch.municipality"]
      
    if(return.type == "character"){
        return(retval$lemma)  
    } else {
        return(retval) 
    }       
}
