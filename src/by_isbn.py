import urllib.request
import json
import textwrap
import pprint

while True:

    base_api_link = "https://www.googleapis.com/books/v1/volumes?q=isbn:"
    user_input = input("Enter ISBN: ").strip()

    with urllib.request.urlopen(base_api_link + user_input) as f:
        text = f.read()

    decoded_text = text.decode("utf-8")
    obj = json.loads(decoded_text) # deserializes decoded_text to a Python object

    pprint.pprint(obj)

    

    #volume_info = obj["items"][0] 
    #authors = obj["items"][0]["volumeInfo"]["authors"]

    # displays title, summary, author, domain, page count and language
    #print("\nTitle:", volume_info["volumeInfo"]["title"])
    #print("\nSummary:\n")
    #print(textwrap.fill(volume_info["searchInfo"]["textSnippet"], width=65))
    #print("\nAuthor(s):", ",".join(authors))
    #print("\nPublic Domain:", volume_info["accessInfo"]["publicDomain"])
    #print("\nPage count:", volume_info["volumeInfo"]["pageCount"])
    #print("\nLanguage:", volume_info["volumeInfo"]["language"])
    #print("\n***")
    #
    #status_update = input("\nEnter another ISBN? y or n: ").lower().strip()

    #if status_update == "n":
    #    print("\nThank you! Have a nice day.")
    #    break

"""
Bible učí, že Ježíš je JEDINOU cestou (spasení vně člověka). New Age učí, že musíme probudit Kristovo vědomí uvnitř sebe.

Bible učí, že Luciver je dábel. New Age učí, že Lucifer (nositel světla, světlonoš) je opravdovým Božím synem.

Bible učí, že musíme uctívat Boha (Stvořitele, Yahweh)
New Age učí, že musíme uctovat to co bylo vytvořeno (nebo i narozeno).

Bible učí, že člověk byl stvořen.
New Age učí, že fyzická stránka člověka se vyvinula a spirituální člověk existoval vždy.

Bible učí, že Bůh NENI součástí stvoření.
New Age učí, že Bůh JE součást stvoření (panteismus).

Bible učí zmrtvýchvstání.
New Age učí reinkarnaci.

Bible učí, že Boží slovo je PRAVDA.
New Age učí, že pravda je uvnitř, v hloubi duše.

Bible náš vede k očekávání druhého příchodu Ježíše Krita, což bude pro všechny viditělné. Jeho slavný příchod je požehnaná naděje.

New Age očekává Maithreu, který bude pomáhat se založením jedne celosvětové vlády, měnového systému a jednoho náboženství.

Bible nás vede k tomu, abychom se ODVRATILI od hříchu.
New Age učí, abychom se odvrátili od neznalosti a ignorance, protože hřích neexistuje.

bible učí o posvěcení v Kristu.
New Age učí k objevení vlastního božství.
"""
