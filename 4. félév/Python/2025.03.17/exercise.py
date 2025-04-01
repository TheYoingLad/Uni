from flask import Flask, request, make_response, render_template, redirect, url_for, abort

# Hozz létre egy Flask webszervert aminek a root címén legyen egy üdvözlő üzenet
app = Flask(__name__)

@app.route("/")
def root():
    return render_template("index2.html")

# Adj hozzá egy üdvözlő oldalt, ahol név szerint köszönti a felhasználót
# @app.route("/user/<name>")
# def user(name):
    # return f"<h1>Welcome {name} </h1>"

# Amikor név szerint üdvözlöd a felhasználót, mentsd el a nevét egy sütibe
@app.route("/user/<name>")
def user(name):
    response = make_response(f"<h1>Welcome {name} </h1>")
    response.set_cookie("name", name)
    return response

# Ha bármilyen olyan url-t szeretne elérni a felhasználó, ami nem létezik, akkor irányítsd át őket az index html-re
# @app.route("/<other>")
# def to_index(other):
#     return render_template("index2.html")

# Az index html-t cseréld le egy üdvözlőoldalra
@app.route("/<other>")
def to_index(other):
    return redirect(url_for("root"))

# Amennyiben a felhasználónak nincsen a sütikben elmentve felhasználóneve, az index html-ről irányítsd át a login.html-re ahonnan csak akkor tudjon ellépni a felhasználó ha megadta a jelszavát


# Az index.html-en legyenek gombok egymás alatt, amik különböző blog bejegyzésekre irányítják át a felhasználót
@app.route("/blogs/<int:blog_id>")
def to_blog(blog_id):
    try:
        return render_template(f"blog_{blog_id}.html")
    except:
        abort(404)

if __name__ == "__main__":
    app.run(debug=True)
