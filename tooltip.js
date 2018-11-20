var currentTooltip = null;

function setCurrentTooltip(tooltip) {
    if (currentTooltip) {
        currentTooltip.style.opacity = 0;
        currentTooltip.style.pointerEvents = 'none';
    }

    if (tooltip) {
        tooltip.style.opacity = 1;
        tooltip.style.pointerEvents = 'auto';
    }

    currentTooltip = tooltip;
}

function showTooltip(e) {
    if (e.target.matches('#text > p > span')) {
        text = e.target.innerText;
        const tooltip = document.querySelector('#tt_' + text);
        if (tooltip && tooltip !== currentTooltip) {
            setCurrentTooltip(tooltip);

            const offsX = 5;
            const offsY = 5;

            tooltip.style.left =
                (e.pageX + tooltip.clientWidth + offsX < document.body.clientWidth)
                    ? (e.pageX + offsX + "px")
                    : (document.body.clientWidth + 5 - tooltip.clientWidth + "px");
            tooltip.style.top =
                (e.pageY + tooltip.clientHeight + offsY < document.body.clientHeight)
                    ? (e.pageY + offsY + "px")
                    : (document.body.clientHeight + 5 - tooltip.clientHeight + "px");
        }
    } else if (e.target.id.includes('tt_')) {
    } else if (currentTooltip) {
        setCurrentTooltip(null);
    }
}

const entries = document.querySelector('#text');
document.addEventListener('mousemove', showTooltip);

