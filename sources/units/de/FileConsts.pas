(* Resource strings for FileUtils and ExtFileTools
   German translation
   
   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Vers. 1.0 - Jan. 2016
   last modified: Dec. 2017
   *)

unit FileConsts;

interface

resourcestring
  rsErrOpening   = 'Fehler beim Öffnen von "%s"';
  rsErrCreating  = 'Fehler beim Erstellen von "%s"';
  rsErrReading   = 'Fehler beim Lesen aus "%s"';
  rsExtractErr   = 'Fehler beim Extrahieren aus "%s"';
  rsErrWriting   = 'Fehler beim Schreiben in "%s"';
  rsErrVerify    = 'Fehler beim Überprüfen von "%s"';
  rsErrClosing   = 'Fehler beim Schließen von "%s"';
  rsErrSetAttr   = 'Fehler beim Setzen der Attribute von "%s"';
  rsErrTimeStamp = 'Fehler beim Setzen der Zeitstempel von "%s"';
  rsErrAcl       = 'Fehler beim Setzen der Berechtigungen von "%s"';
  rsErrDirEntry  = 'Fehler beim Erstellen eines Verzeichniseintrags: "%s"';
  rsErrAddDir    = 'Fehler beim Hinzufügen eines Verzeichnisses zu "%s"';
  rsErrAddFile   = 'Fehler beim Hinzufügen einer Datei zu "%s"';
  rsErrAddStream = 'Fehler beim Hinzufügen eines Datenstroms zu "%s"';
  rsErrCompFile  = 'Fehler beim Vergleichen einer Datei aus "%s" mit "%s"';
  rsErrEndRecord = 'Fehler beim Schreiben des Endblocks';
  rsErrNotFound  = 'Datei nicht gefunden: "%s"';
  rsErrSystem    = 'System-Fehler: %s';

  rsError        = 'Fehler - ';
  rsError2       = 'Fehler(2) - ';
  rsWarning      = 'Warnung - ';
  rsInfo         = 'Info - ';
  rsFileCreate   = 'Datei konnte nicht erstellt werden';
  rsFileOpen     = 'Datei konnte nicht geöffnet werden';
  rsFileClose    = 'Datei konnte nicht geschlossen werden';
  rsFileRead     = 'Aus der Datei konnte nicht gelesen werden';
  rsFileWrite    = 'In die Datei konnte nicht geschrieben werden';
  rsFileAttr     = 'Die Attribute konnten nicht gesetzt werden';
  rsFileFull     = 'Zu wenig Platz auf dem Datenträger';
  rsFileGZip     = 'Ungültiger Datei-Header';
  rsFileCheck    = 'Beschädigte Datei';
  rsFileECrypt   = 'Die Verschlüsselung ist fehlgeschlagen';
  rsFileDCrypt   = 'Die Entschlüsselung ist fehlgeschlagen';
  rsFileVerify   = 'Die Überprüfung ist fehlgeschlagen';
  rsLongPath     = 'Pfad zu lang';
  rsNotFound     = 'Datei nicht gefunden';
  rsFileTS       = 'Der Zeitstempel konnte nicht gesetzt werden';
  rsStorage      = 'Fehler beim Kopieren der Dokumentzusammenfassung';
  rsTimeout      = 'Zeitüberschreitung beim Kopieren der Datei';
  rsStream       = 'Undefinierter Stream';
  rsAcl          = 'Die Berechtigungen konnten nicht kopiert werden';
  rsFileExists   = 'Datei bereits vorhanden';
  rsSzMismatch   = 'Unterschiedliche Größe';
  rsVerOpen      = 'Referenz-Datei konnte nicht geöffnet werden';
  rsDirCreate    = 'Verzeichnis konnte nicht erstellt werden';
  rsFtpRead      = 'Über FTP konnte nicht gelesen werden';
  rsFtpWrite     = 'Über FTP konnte nicht geschrieben werden';
  rsFtpConnect   = 'Die FTP-Verbindung konnte nicht aufgebaut werden';
  rsFtpBroken    = 'Die FTP-Verbindung wurde vom Server beendet';
  rsFtpDatConn   = 'Die FTP-Datenverbindung konnte nicht hergestellt werden';
  rsFtpTimeout   = 'Zeitüberschreitung bei Kopieren per FTP';
  rsCompare      = 'Unterschiedlicher Inhalt';
  rsZipCrSeg     = 'Fehler beim Erstellen eines neuen Zip-Segments';
  rsSignature    = 'Ungültige Zip-Signatur';
  rsExtract      = 'Datei konnte nicht extrahiert werden';
  rsFormat       = 'Nicht unterstütztes Dateiformat';
  rsTmpFile      = 'Temporäre Datei konnte nicht umbenannt werden';
  rsZipRdSeg     = 'Fehler beim Lesen des nächsten Zip-Segments';
  rsAltStreams   = 'Fehler beim Kopieren von alternativen Datenströmen';
  rsFileDel      = 'Fehler beim Löschen einer Datei';
  rsFileRen      = 'Fehler beim Umbenennen einer Datei';

  rsUserBreak    = 'Abbruch durch den Benutzer';
  rsUnknownErrCode = 'Unbekannter Fehlercode ($%.8x)';
  rsCopy         = ' (Kopieren)';
  rsGZip         = ' (GZ packen)';
  rsGUnzip       = ' (GZ entpacken)';
  rsZip          = ' (Zip packen)';
  rsUnzip        = ' (Zip entpacken)';
  rsEnCrypt      = ' (Verschlüsseln)';
  rsDeCrypt      = ' (Entschlüsseln)';

  rsNoFileInfo   = 'Datei-Informationen sind nicht verfügbar';
  rsDescription  = 'Description: ';
  rsCompany      = 'Firma: ';
  rsCopyright    = 'Copyright: ';
  rsVersion      = 'Dateiversion: ';
  rsFileDate     = 'Zuletzt geändert: ';

  rsStrFormatError  = 'Format-Fehler: ';

implementation

end.
