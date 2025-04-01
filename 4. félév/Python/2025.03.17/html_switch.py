from flask import Flask, render_template, abort

app = Flask(__name__)

@app.route("/")
def index():
    return render_template(f"index.html")

@app.route("/other")
def index():
    return render_template(f"other_page.html")

if __name__ == "__main__":
    app.run(debug=True)
