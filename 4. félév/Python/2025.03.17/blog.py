from flask import Flask, render_template, abort

app = Flask(__name__)

@app.route("/blogs/<int:blog_id>")
def show_blog(blog_id):
    try:
        return render_template(f"blog_{blog_id}.html")
    except:
        abort(404)

if __name__ == "__main__":
    app.run(debug=True)
