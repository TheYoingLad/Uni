from flask import Flask

app = Flask(__name__)

@app.route("/")
def hello():
    return "heloo"

@app.route("/welcome")
def welcome():
    return "welcome user, it's good to have you here"

if __name__ == "__main__":
    app.run(debug=True)
