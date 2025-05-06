from flask import Flask

app = Flask(__name__)

@app.route("/")
def hello():
    return "heloo"

@app.route("/welcome/<name>")
def welcome(name):
    return f"welcome {name}, it's good to have you here"

if __name__ == "__main__":
    app.run(debug=True)
