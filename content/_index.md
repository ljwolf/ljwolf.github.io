---
# === Required fields  ===
# Your name 
name: "Levi John Wolf"
# Your profile picture
imgname: 
  name: "image/main.jpg"
  alt: "Picture of me"
  type: image/jpeg
# More sources can be added (optional) using 
# imgOther:
#   - name: $IMAGE_PATH
#     type: $IMAGE_TYPE
#   - name: $IMAGE_PATH
#     type: $IMAGE_TYPE
# ...
# A title (job title or "Researcher", "PhD student", etc.)
#personal_title: "Geographer & Developer"
# An address (you can list multiple)
address: 
  - 
    name: Spatial Analyst
    street: University of Bristol 
    postal_code: United Kingdom

# === Optional fields ===
# Add an email with a mailto: hyperlink
# email: aaaa@example.com
# Add an email "image" for spam protection. With light and dark mode
# emailImg: 
#   dark: /img/dark_email.png
#   light: /img/light_email.png

# List your publications. The required fields are pdf, title, and image 
# (which should be the image path). The other fields are optional.
#publications:
#  - 
#    authors:
#        - name: Rothe, S. 
#          me: true
#        - name: Andreyev, A. N. 
#        - name: Antalic, S.
#        - name: Borschevsky, A.
#        - name: Capponi, L.
#        - name: Cocolios, T. E.
#        - name: De Witte, H.
#        - name: Eliav, E.
#    title: "Measurement of the First Ionization Potential of Astatine by Laser Ionization Spectroscopy"
#    # Will write "In ${journal}, ${date}"
#    date: 2013
#    journal: Nature Communications
#    image: img/paper_illustration.png
#    # A bibtex (or any other format) citation that people can copy directly from the website.
#    citation: "@article{article,\n
#author = {Rothe, Sebastian and Andreyev, A and Antalic, Stanislav and Borschevsky, Anastasia and Capponi, Luigi and Cocolios, Thomas and De Witte, Hilde and Eliav, Ephraim and Fedorov, D.V. and Fedosseev, Valentin and Fink, D and Fritzsche, s and Ghys, Lars and Huyse, M and Imai, Nobuaki and Kaldor, U and Kudryavtsev, Yu and Koester, Ulli and Lane, J and Wendt, Klaus},\n
#year = {2013},\n
#month = {05},\n
#pages = {1835},\n
#title = {Measurement of the first ionization potential of astatine by laser ionization spectroscopy},\n
#volume = {4},\n
#journal = {Nature communications},\n
#doi = {10.1038/ncomms2819}\n
#}"
#    pdf: https://www.nature.com/articles/ncomms2819.pdf
#    # A list of link that will appear as badges at the bottom of the publication.
#    links:
#      -
#        name: Main URL
#        url: "https://www.nature.com/articles/ncomms2819"
#      -
#        name: ResearchGate
#        url: "https://www.researchgate.net/publication/236836716_Measurement_of_the_first_ionization_potential_of_astatine_by_laser_ionization_spectroscopy"
#    # A description for the paper.
#    description: The radioactive element astatine exists only in trace amounts in nature. Its properties can therefore only be explored by study of the minute quantities of artificially produced isotopes or by performing theoretical calculations. One of the most important properties influencing the chemical behaviour is the energy required to remove one electron from the valence shell, referred to as the ionization potential.
---

I am an American expat currently working as an Associate Professor in Spatial Analysis at the University of Bristol's Quantitative Spatial Science Lab. I work in spatial data science, building new methods and software to learn new things about social and environmental processes. [Spatial analysis](https://en.wikipedia.org/wiki/Spatial_analysis) is a way of using the *spatial relationships* between things in a system to build a better understanding *of* that system than would be possible if we studied the elements in isolation. The techniques I develop are used across urban planning, political science, economics, public health, astronomy, and genomics to understand things like commuting, infill development, deprivation, voting, redistricing, industrial clusters, disease transmission, and anomaly detection in images. 

So, maybe it's easier to give a few examples of work I've done: I've worked on detecting gerrymandering [[1](https://www.nsf.gov/awardsearch/showAward?AWD_ID=1657689&HistoricalAwards=false#_=_),[2](bce)], [neighborhood social change](https://journals.sagepub.com/doi/10.1177/2399808319875752), [local statistical models](https://onlinelibrary.wiley.com/doi/full/10.1111/gean.12147), [affordable rent](https://www.bristol.gov.uk/council-homes/tackling-the-rent-crisis), [bayesian computation](https://doi.org/10.1111/gean.12135), [species distribution modelling](https://research-information.bris.ac.uk/en/projects/revealing-coccolithophore-trait-diversity-and-its-climatic-impact)... all as a *spatial analyst*. In future, I'm broadly interested in a few different topics. 

1. Classical statistical and machine learning methods assume that we're all independent of one another; that what you do does not affect your neighbor and *vice versa*. As a spatial analyst, I am very interested in the fact that our surroundings affect our behavior. So, it's important to integrate spatial reasoning into data science techniques. This either looks like traditional spatial data science techniques that use augmented spatial information, or involves the development of entirely new spatial statistical models \& data science methods. Some people call this "**GeoAI**", or "**spatial machine learning**", or "**geographic data science**." I'm interested in all of those things, and have many publications in this area.  
2. I am always interested in working with students on questions about **redistricting and election forecasting.** This is my main area of public activity in geography, where I have engaged fairly extensively in citizens redistricting processes. So, I'd accept students readily who want to study geography and spatial structure of elections, partisan swing, redistricting, and voter realignment.
3. **Causal inference** is a really big part of contemporary science, but spatial relationships can really mess with the statistical tools we have to analyze causal relationships. If you're interested in working on methods for spatial causal inference, I'm your guy. 
4. **Housing** is another area where I am increasingly interested. In particular, renting and **rent stabilization policy** is core to how cities "work", and it's important for us to understand the impacts of rent and rental policy. I am very interested in accepting students on this topic. 
5. And finally, I am open to new collaborations, students, and work on **systems of cities**. I am very interested in the distributional dynamics of city systems: why do certain places grow and decline, while others seem to grow without bound? How does social inequality affect urbanisation, and vice versa?  

I've worked at [Nextdoor](https://nextdoor.com) and [CARTO](https://carto.com) (twice), and currently consult for [MondialRelay](https://www.mondialrelay.fr) on spatial optimziation. All have been great places to work. I am one of a few core maintainers of [geopandas](https://geopandas.org), the main library for representing and working with geographic data in Python. I'm the author of [cenpy](https://github.com/ljwolf/cenpy), a wrapper for the US Census Bureau data API that discovers & updates itself when the API changes. I also am a co-maintainer of the [Python Spatial Analysis Library](https://github.com/pysal/pysal) (the main spatial stats library in Python) and [contextily](https://github.com/darribas/contextily) (a library to add basemap tiles to your python maps). 

I have written a book with my colleagues [Dani Arribas-Bel](https://darribas.org) and [Sergio Rey](https://sergerey.org) on **geographic data science**. You can read it at [`geographicdata.science/book`](https://geographicdata.science/book). I'm also editing a book with Rich Harris and Alison Heppenstall, called a [*Research Agenda for Spatial Analysis*](https://ljwolf.org/rasa), expected out in Spring of 2024. And, [Nicholas Dorward](https://research-information.bris.ac.uk/en/persons/nicholas-m-dorward) and I are preparing *Causal Inference in Spatial Analysis* for SAGE, expected Fall of 2025. Finally, check out my podcast with Rachel Franklin and Daniel Arribas-Bel, [the GLaD podcast, your spatial fix for **G**eography, **L**ife, **a**nd **D**ata.](https://open.spotify.com/show/3n79ptWoAM55YfHoTuBfHu?si=91b941c54e274316&nd=1)

I am available to consult on spatial analysis, modelling, and optimisation problems. 
