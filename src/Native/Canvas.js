const canvases = [];

export function initCanvas(app, port) {
    const canvas = {};
    loadAssets(app, port.assets, port, canvas);
    canvas.container = document.getElementById(port.id);
    canvas.id = port.id;
    if (canvas.container == null)
        throw `Canvas with an id ${port.id} doesn't exists. Maybe you forgot to include canvas to the view.`;
    canvases.push(canvas);
}

function loadAssets(app, assets, port, canvas) {
    const img = new Image();

    if (assets.length === 0) {

    }
    else if (assets.length === 1) {
        const asset = assets[0];
        img.onload = function () {
            const obj = {id: asset.id, value: img};
            canvas.images.push(obj);
            app.ports.onInitCanvas.send(port);
        };
        img.src = asset.url;
    }
    else {
        const asset = assets[0];
        img.onload = function () {
            assets.shift();
            const obj = {id: asset.id, value: img};
            canvas.images.push(obj);
            loadAssets(app, assets, port);
        };
        img.src = asset.url;
    }
}


export function render(stage) {
    const context = canvases.find(c => c.id === stage.id).container.getContext('2d');
    stage.operations.forEach(op => {
        draw(context, op)
    });
}


export function getImageData(input, app) {
    const source = canvases.find(c => c.id === input.id).container;
    const destination = document.createElement('canvas');
    destination.width = input.width;
    destination.height = input.height;

    const context = destination.getContext('2d');
    context.drawImage(source, input.x, input.y, destination.width, destination.height);

    const imageData = context.getImageData(input.x, input.y, input.width, input.height);
    app.ports.onGetImageData.send({url: destination.toDataURL("image/png"), data: Array.from(imageData.data)});

}


const draw = function (ctx, op) {
    const value = op.value;

    switch (op.type) {
        case "SKIP":
            break;
        case "BEGIN_PATH":
            ctx.beginPath();
            break;
        case "CLOSE_PATH":
            ctx.closePath();
            break;
        case "FILL_PATH":
            ctx.fill(value);
            break;
        case "STROKE_PATH":
            ctx.stroke();
            break;
        case "MOVE_TO":
            ctx.moveTo(value.x, value.y);
            break;
        case "LINE_TO":
            ctx.lineTo(value.x, value.y);
            break;
        case "FILL_RECT":
            ctx.fillRect(value.x, value.y, value.width, value.height);
            break;
        case "STROKE_RECT":
            ctx.strokeRect(value.x, value.y, value.width, value.height);
            break;
        case "CLEAR_RECT":
            ctx.clearRect(value.x, value.y, value.width, value.height);
            break;
        case "ARC":
            ctx.arc(value.x, value.y, value.radius, value.startAngle, value.endAngle, value.anticlockwise);
            break;
        case "ARC_TO":
            ctx.arcTo(value.x1, value.y1, value.x2, value.y2, value.radius);
            break;
        case "QUADRATIC_CURVE_TO":
            ctx.quadraticCurveTo(value.cpx, value.cpy, value.x, value.y);
            break;
        case "BEZIER_CURVE_TO":
            ctx.bezierCurveTo(value.cp1x, value.cp1y, value.cp2x, value.cp2y, value.x, value.y);
            break;
        case "RECT":
            ctx.rect(value.x, value.y, value.width, value.height);
            break;
        case "STROKE_STYLE":
            ctx.strokeStyle = getColor(ctx, value);
            break;
        case "FILL_STYLE":
            ctx.fillStyle = getColor(ctx, value);
            break;
        case "GLOBAL_ALPHA":
            ctx.globalAlpha = value;
            break;
        case "LINE_WIDTH":
            ctx.lineWidth = value;
            break;
        case "LINE_CAP":
            ctx.lineCap = value;
            break;
        case "LINE_JOIN":
            ctx.lineJoin = value;
            break;
        case "MITER_LIMIT":
            ctx.miterLimit = value;
            break;
        case "LINE_DASH":
            ctx.setLineDash(value);
            break;
        case "FILL_TEXT":
            if (value.maxWidth > -1)
                ctx.fillText(value.text, value.x, value.y, value.maxWidth);
            else
                ctx.fillText(value.text, value.x, value.y);
            break;
        case "STROKE_TEXT":
            if (value.maxWidth > -1)
                ctx.strokeText(value.text, value.x, value.y, value.maxWidth);
            else
                ctx.strokeText(value.text, value.x, value.y);
            break;
        case "FONT":
            ctx.font = value;
            break;
        case "TEXT_BASELINE":
            ctx.textBaseline = value;
            break;
        case "TEXT_ALIGN":
            ctx.textAlign = value;
            break;
        case "TEXT_DIRECTION":
            ctx.direction = value;
            break;
        case "SHADOW":
            ctx.shadowOffsetX = value.offsetX;
            ctx.shadowOffsetY = value.offsetY;
            ctx.shadowBlur = value.blur;
            ctx.shadowColor = value.color;
            break;
        case "DRAW_IMAGE":
            try {
                ctx.drawImage(images.find(img => img.id === value.id).value, value.x, value.y);
            }
            catch (err) {
                throw `I am sorry, but I Cannot find asset with an id: \"${value.id}\" `
            }
            break;
        case "SCALE_IMAGE":
            try {
                ctx.drawImage(images.find(img => img.id === value.id).value, value.x, value.y, value.width, value.height);
            }
            catch (err) {
                throw `I am sorry, but I Cannot find asset with an id: \"${value.id}\" `
            }
            break;
        case "SLICE_IMAGE":
            try {
                ctx.drawImage(images.find(img => img.id === value.id).value, value.sx, value.sy, value.sWidth, value.sHeight, value.dx, value.dy, value.dWidth, value.dHeight);
            }
            catch (err) {
                throw `I am sorry, but I Cannot find asset with an id: \"${value.id}\" `
            }
            break;
        case "SMOOTHING":
            ctx.mozImageSmoothingEnabled = value;
            ctx.webkitImageSmoothingEnabled = value;
            ctx.msImageSmoothingEnabled = value;
            ctx.imageSmoothingEnabled = value;
            break;
        case "SAVE":
            ctx.save();
            break;
        case "RESTORE":
            ctx.restore();
            break;
        case "TRANSLATE":
            ctx.translate(value.x, value.y);
            break;
        case "ROTATE":
            ctx.rotate(value);
            break;
        case "SCALE":
            ctx.scale(value.width, value.height);
            break;
        case "TRANSFORM":
            ctx.transform(value.a, value.b, value.c, value.d, value.e, value.f);
            break;
        case "COMPOSITION":
            ctx.globalCompositeOperation = value;
            break;
        case "CLIP":
            ctx.clip(value);
            break;
    }
};


const getColor = function (ctx, value) {
    var result = null;
    const val = value.value;
    switch (value.type) {
        case "PLAIN":
            result = val;
            break;
        case "PATTERN":
            try {
                result = ctx.createPattern(images.find(img => img.id === val.id).value, val.repetition);
            }
            catch (err) {
                throw `I am sorry, but I Cannot find asset with an id: \"${val.id}\" `
            }
            break;
        case "LINEAR_GRADIENT":
            const linear = ctx.createLinearGradient(val.x1, val.y1, val.x2, val.y2);
            val.stops.forEach(stop => {
                linear.addColorStop(stop.value, stop.color);
            });
            result = linear;
            break;
        case "RADIAL_GRADIENT":
            const radial = ctx.createRadialGradient(val.x1, val.y1, val.r1, val.x2, val.y2, val.r2);
            val.stops.forEach(stop => {
                radial.addColorStop(stop.value, stop.color)
            });
            result = radial;
    }
    return result;
};

