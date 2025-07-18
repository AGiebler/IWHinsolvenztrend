test_that("identify_glaeubigerausschuss correcly from text", {

    text1 <- "Mit Eröffnung des Insolvenzverfahrens ist die Tätigkeit des mit Beschluss vom 
              20.05.2025 eingesetzten vorläufigen Gläubigerausschusses beendet. Es wird ein neuer 
              vorläufiger Gläubigerausschuss eingesetzt."
    text2 <- "Mit Eröffnung des Insolvenzverfahrens ist die Tätigkeit des mit Beschluss vom 
              20.05.2025 eingesetzten vorläufigen Gläubigerausschusses beendet."

    expect_true(identify_glaeubigerausschuss(text1))
    expect_false(identify_glaeubigerausschuss(text2))

})


test_that("identify_vorl_insolvenzverwalter correctly from text", {

    text1 <- "Zum vorläufigen Insolvenzverwalter wird bestellt: Rechtsanwalt Marcus Voigt, 
              Fürther Straße 62, 90429 Nürnberg, Telefon: +49(911)312203, Telefax: +49(911)325523.
              wird gemäß § 21 Abs. 2 S. 1 Nr. 2 Alt. 2 InsO angeordnet, dass Verfügungen des 
              Schuldners nur mit Zustimmung des vorläufigen Insolvenzverwalters wirksam sind."
    text2 <- "Zur vorläufigen Insolvenzverwalterin wird bestellt: Rechtsanwalt Marcus Voigt, 
              Fürther Straße 62, 90429 Nürnberg, Telefon: +49(911)312203, Telefax: +49(911)325523.
              wird gemäß § 21 Abs. 2 S. 1 Nr. 2 Alt. 2 InsO angeordnet, dass Verfügungen des 
              Schuldners nur mit Zustimmung des vorläufigen Insolvenzverwalters wirksam sind."
    text3 <- "Zum Insolvenzverwalter wird bestellt: Rechtsanwalt Marcus Voigt, 
              Fürther Straße 62, 90429 Nürnberg, Telefon: +49(911)312203, Telefax: +49(911)325523.
              wird gemäß § 21 Abs. 2 S. 1 Nr. 2 Alt. 2 InsO angeordnet, dass Verfügungen des 
              Schuldners nur mit Zustimmung des vorläufigen Insolvenzverwalters wirksam sind."

    expect_true(identify_vorl_insolvenzverwalter(text1))
    expect_true(identify_vorl_insolvenzverwalter(text2))
    expect_false(identify_vorl_insolvenzverwalter(text3))
})


test_that("identify_sachwalter correctly from text", {

    text1 <- "Es wird Eigenverwaltung angeordnet.3. Die Schuldnerin ist berechtigt, 
              unter Aufsicht des Sachwalters die Insolvenzmasse zu verwalten und über sie zu verfügen 
              (§ 270 Abs. 1 Satz 1 InsO).4. Zum Sachwalter wird bestellt: 
              Rechtsanwalt Martin MuchaReinsburgstraße 27, 70178 Stuttgart
              Telefon: 0711 96689-0Telefax: 0711 96689-395."
    text2 <- "Es wird Eigenverwaltung angeordnet.3. Die Schuldnerin ist berechtigt, 
              unter Aufsicht des Sachwalters die Insolvenzmasse zu verwalten und über sie zu verfügen 
              (§ 270 Abs. 1 Satz 1 InsO).4. Zur Sachwalterin wird bestellt: 
              Rechtsanwalt Martin MuchaReinsburgstraße 27, 70178 Stuttgart
              Telefon: 0711 96689-0Telefax: 0711 96689-395."
    text3 <- "Es wird Eigenverwaltung angeordnet.3. Die Schuldnerin ist berechtigt, 
              unter Aufsicht des Sachwalters die Insolvenzmasse zu verwalten und über sie zu verfügen."

    expect_true(identify_sachwalter(text1))
    expect_true(identify_sachwalter(text2))
    expect_false(identify_sachwalter(text3))

})


test_that("identify_eroeffnung correctly from text", {
    text1 <- "Amtsgericht Bielefeld, Aktenzeichen: 43 IN 192/25 Über das Vermögen der im Handelsregister 
              des Amtsgerichts Bad Oeynhausen unter HRB 18920 eingetragenen BIOXYD GmbH, Am Spitzenend 69, 32479 Hille, 
              gesetzlich vertreten durch die Geschäftsführer Herrn Michael-Adam Zurek, und Herrn Dustin Zurek,
              Geschäftszweig: Fahrzeugfolierung, Car Design und Branding, Chip Tuning, Online Shop, Videography, 
              Verkauf von Fahrzeugteilen, Laminierung von Fahrzeugteilen wird wegen Zahlungsunfähigkeit und 
              Überschuldung heute, am 30.04.2025, um 11:23 Uhr das Insolvenzverfahren eröffnet. Die Eröffnung erfolgt 
              aufgrund des am 28.02.2025 bei Gericht eingegangenen Antrags der Schuldnerin"
    text2 <- "Amtsgericht Hamburg, Aktenzeichen: 67c IN 146/25 In dem Insolvenzeröffnungsverfahren 
              über das Vermögen der im Handelsregister des Amtsgerichts Hamburg unter HRB 135863 eingetragenen 
              LCA 24 Logistik GmbH, Liebigstraße 94, 22113 Hamburg, gesetzlich vertreten durch die 
              Geschäftsführerin Frau Ilknur GürcanGeschäftszweig: Gegenstand des Unternehmens ist 
              Logistik Containerbe- und entladung nebst allen damit im Zusammenhang stehenden Tätigkeiten 
              aller Art, soweit diese nicht einer besonderen Erlaubnis bedürfen sind die am 26.05.2025 
              angeordneten Sicherungsmaßnahmen durch Beschluss vom 30.06.2025 aufgehoben worden. 
              67c IN 146/25Amtsgericht Hamburg, 30.06.2025"

    expect_true(identify_eroeffnung(text1))
    expect_false(identify_eroeffnung(text2))
})


test_that("identify_abweisung correctly from text", {
    text1 <- "12 IN 112/24: In dem Insolvenzantragsverfahren über das Vermögen der In O Boxes GmbH, 
              Grasweg 4-6, 27607 Geestland, vertr. d. den Geschäftsführer Ferhat Hogir Özel 
              (AG Tostedt, HRB 210861), vertr. d.: Ferhat Hogir Özel, Tajenfeld 2a, 27607 Geestland, 
              (Geschäftsführer), ist der Antrag auf Eröffnung des Insolvenzverfahrens am 24.06.2025 
              mangels Masse abgewiesen worden, § 26 Abs. 1 InsO."
    text2 <- "601 IN 301/23|In dem Verfahren über den Antragauf Eröffnung des Insolvenzverfahrens über 
              den Nachlass d.Schmidbauer Matthias, geboren am 24.07.1958, verstorben am 05.10.2021, 
              zuletzt wohnhaft: Gottlob Weiler Straße 28, 83052 Bruckmühl- Erblasser -|Beschluss:
              Der Antrag auf Eröffnung des Insolvenzverfahrens über den Nachlass wird mangels Masse abgewiesen"
    text3 <- "Amtsgericht Bielefeld, Aktenzeichen: 43 IN 192/25 Über das Vermögen der im Handelsregister 
              des Amtsgerichts Bad Oeynhausen unter HRB 18920 eingetragenen BIOXYD GmbH, Am Spitzenend 69, 32479 Hille, 
              gesetzlich vertreten durch die Geschäftsführer Herrn Michael-Adam Zurek, und Herrn Dustin Zurek,
              Geschäftszweig: Fahrzeugfolierung, Car Design und Branding, Chip Tuning, Online Shop, Videography, 
              Verkauf von Fahrzeugteilen, Laminierung von Fahrzeugteilen wird wegen Zahlungsunfähigkeit und 
              Überschuldung heute, am 30.04.2025, um 11:23 Uhr das Insolvenzverfahren eröffnet. Die Eröffnung erfolgt 
              aufgrund des am 28.02.2025 bei Gericht eingegangenen Antrags der Schuldnerin"

    expect_true(identify_abweisung(text1))
    expect_true(identify_abweisung(text2))
    expect_false(identify_abweisung(text3))
})