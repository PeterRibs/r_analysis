const backdropElement = document.getElementById('backdrop');
const modalLinkElements = document.querySelectorAll('.info-modal');
console.log(modalLinkElements)
let infoModal;

function toggleBackdrop() {
    backdropElement.classList.toggle('visible');
}

function presentInfoModal(event) {
    const figure = event.target;
    const figureSRC = figure.src;
    console.log(figureSRC)
    toggleBackdrop();
    infoModal = document.createElement('div');
    infoModal.classList.add('modal');
    infoModal.innerHTML = `
    <img alt="fig" src= ${figureSRC}></img>
    `;

    console.log(infoModal)
    document.body.append(infoModal);
    console.log(document.body)
}


function hideInfoModal() {
    toggleBackdrop();
    document.body.removeChild(infoModal);
}

for (const linkElement of modalLinkElements) {
    linkElement.addEventListener('click', presentInfoModal);
    console.log(linkElement)
}

backdropElement.addEventListener('click', hideInfoModal);