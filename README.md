# SAD-2025
Progetto di Statistica ed analisi dei dati Torino

Il dataset scelto per questo studio è il dataset “Iranian Churn”. 
Il Churn rate, o abbandono dei clienti, è una metrica importante che le aziende devono monitorare quando cercano di espandere il proprio business. 
Questa metrica rappresenta il numero di clienti che hanno smesso di utilizzare il prodotto o il servizio durante un determinato periodo di tempo. 
In ultima analisi, il Churn rate di un’azienda identificherà il tasso complessivo di retention dei clienti.
Il Churn rate è inversamente correlato al tasso di retention dei clienti. Un'azienda con un alto Churn rate avrà un basso tasso di retention, il che significa che non riesce a mantenere i clienti. Al contrario, un basso Churn rate implica un alto tasso di retention, suggerendo che l'azienda è efficace nel mantenere i suoi clienti nel tempo.
Monitorare il Churn rate aiuta le aziende a identificare aree di miglioramento e a sviluppare strategie per aumentare la soddisfazione e la fidelizzazione dei clienti.
Questo dataset è stato raccolto in modo casuale dal database di una compagnia telefonica iraniana nel corso di 12 mesi. 
Contiene un totale di 3150 righe di dati, ciascuna rappresentante un cliente, e presenta informazioni su 14 colonne. Le variabili presenti in questo dataset includono:

•	Call Failures (Fallimenti di Chiamata): numero di fallimenti di chiamata del fruitore;
•	Complains (Lamentela): Lamentele riportate dal fruitore;
•	Subscription Length (Durata della sottoscrizione): totale mesi di fruizione del servizio;
•	Charge Amount (Importo addebitato): fascia di costo del servizio mensile;
•	Seconds of Use (Secondi di Utilizzo): totale secondi di chiamate effettuate dagli utenti;
•	Frequency of use (Frequenza di Utilizzo): numero totale di chiamate da parte del fruitore del servizio (questa è un’assunzione dato che dalla documentazione non è chiaro. È stata data una spiegazione con l’analisi bivariata);
•	Frequency of SMS (Frequenza di SMS): numero totale di messaggi di testo da parte del fruitore del servizio;
•	Distinct Called Numbers (Numeri Chiamati Distinti): numero totale di chiamate distinte da parte del fruitore del servizio;
•	Age Group (Gruppo di Età): gruppo d’età a cui appartiene il fruitore del servizio;
•	Tariff Plan (Piano Tariffario): piano tariffario del servizio specifico dell’utente;
•	Status (Stato): stato dell’attivazione del servizio ;
•	Age (Età): Età del fruitore del servizio;
•	Churn (Abbandono): Abbandono del servizio da parte dell’utente;
•	Customer Value (Valore del Cliente): il valore calcolato del cliente considerando le feature associategli (questa è un’assunzione dato che non è chiaro nella documentazione esposta dai creatori del dataset come questa feature sia stata calcolata tramite l’analisi bivariata è stata data una spiegazione);

Tutte le variabili, eccetto l'attributo churn (abbandono), sono dati aggregati dei primi 9 mesi. Le etichette di churn indicano lo stato dei clienti alla fine dei 12 mesi. I tre mesi rappresentano un intervallo di pianificazione designato.
