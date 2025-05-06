from fastapi import FastAPI, HTTPException, Request
import uvicorn
from starlette.responses import FileResponse 
import io
import matplotlib.pyplot as plt
from fastapi.responses import Response



app = FastAPI()

@app.get("/")
async def read_index():
    return FileResponse('templates/index.html')

@app.get("/plot")
async def get_plot():
    # Create a Matplotlib figure
    fig, ax = plt.subplots()
    ax.plot([1, 2, 3, 4], [10, 20, 25, 30], marker='o', linestyle='-', color='b')
    ax.set_title("Sample Plot")
    ax.set_xlabel("X-axis")
    ax.set_ylabel("Y-axis")

    # Save the plot to a BytesIO buffer
    buffer = io.BytesIO()
    plt.savefig(buffer, format="png")
    plt.close(fig)  # Close the figure to free memory
    buffer.seek(0)  # Move the pointer to the start of the stream

    # Return the image as a response
    return Response(buffer.getvalue(), media_type="image/png")


if __name__ == "__main__":
    uvicorn.run(app, host="0.0.0.0", port=10000)