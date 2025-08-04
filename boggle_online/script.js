const DICE = [
  'BILART', 'ELRWUI', 'MPDCAE', 'MOISRA', 'TAEOAI', 'EPULST', 'ZNEADV',
  'EDSNOT', 'AIRFOX', 'OMARSI', 'RATIBL', 'ESIHRN', 'QBMAJO', 'EITAOA',
  'ESIEHF', 'NKETOU', 'UTESLP', 'OBAMJQ', 'ELUNYG', 'RALSCE', 'UTOKEN',
  'IFEHES', 'VNEITG', 'MAPDEC', 'OSDETN',

]
window.onload = () => {
  let board = document.querySelector('.board');
  for (let i = 0; i < DICE.length; i++) {
    let dice = document.createElement('ul');
    dice.innerHTML = ''
    for (let j = 0; j < 6; j++) {
      dice.innerHTML +=
          '<li ' + ((j == 0) ? '' : 'hidden') + '>' + DICE[i][j] + '</li>';
    }
    board.appendChild(dice);
  }
}