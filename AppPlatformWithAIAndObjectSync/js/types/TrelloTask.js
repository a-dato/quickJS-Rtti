import {Object_, Provider_, TypeDescriptor_, registerType} from '../platform.js';

export class TrelloTask extends Object_ {
  ToString() {
    return this.Name;
  }
}

class TrelloTaskProvider extends Provider_ {
  async Data(filter) {
    if (!tokens['TrelloKey'] || !tokens['TrelloCredentials']) {
      return [];
    }

    const auth = 'key=' + encodeURIComponent(String(tokens['TrelloKey']))
      + '&token=' + encodeURIComponent(String(tokens['TrelloCredentials']));

    async function read(path) {
      let response;
      try {
        response = await fetch('https://api.trello.com/1/' + path
          + (path.indexOf('?') >= 0 ? '&' : '?') + auth);
      } catch (error) {
        throw new Error('Trello service could not be reached.');
      }
      if (!response.ok) {
        throw new Error('Trello request failed (HTTP ' + Number(response.status)
          + '). Check TrelloKey and TrelloCredentials and board access.');
      }
      let data;
      try {
        data = await response.json();
      } catch (error) {
        throw new Error('Trello returned an incompatible JSON response.');
      }
      if (!Array.isArray(data)) {
        throw new Error('Expected a Trello collection.');
      }
      return data;
    }

    const boards = await read('members/me/boards');
    const tasks = [];
    const seen = new Set();
    for (const board of boards) {
      const boardID = String(board.id || '');
      if (!/^[a-f0-9]{24}$/.test(boardID)) {
        throw new Error('Trello returned an invalid board identifier.');
      }
      let before = '';
      while (true) {
        const path = 'boards/' + encodeURIComponent(boardID)
          + '/cards?filter=all&fields=name,desc,url,idBoard,closed'
          + (before ? '&before=' + encodeURIComponent(before) : '');
        const cards = await read(path);
        if (!cards.length) {
          break;
        }
        let oldest = '';
        for (const card of cards) {
          const id = String(card.id || '');
          if (!/^[a-f0-9]{24}$/.test(id) || card.name == null
              || card.desc == null || !card.url
              || String(card.idBoard) !== boardID
              || typeof card.closed !== 'boolean') {
            throw new Error('Trello returned incompatible card fields.');
          }
          if (!oldest || id < oldest) {
            oldest = id;
          }
          if (!seen.has(id)) {
            seen.add(id);
            tasks.push({
              Name: String(card.name),
              CardID: id,
              Description: String(card.desc),
              URL: String(card.url),
              BoardID: boardID,
              Closed: Boolean(card.closed),
              ProjectID: ''
            });
          }
        }
        if (before && oldest >= before) {
          throw new Error('Trello pagination did not advance.');
        }
        before = oldest;
      }
    }
    return tasks;
  }
}

class TrelloTaskType extends TypeDescriptor_ {
  constructor() {
    super();
    this.Provider = new TrelloTaskProvider();
    this.PropertyDescriptor = {
      Name: { Type: 'string' },
      CardID: { Type: 'string' },
      Description: { Type: 'string' },
      URL: { Type: 'string' },
      BoardID: { Type: 'string' },
      Closed: { Type: 'boolean' },
      ProjectID: { Type: 'string' }
    };
  }

  get StorageName() {
    return 'TrelloTasks';
  }

  CreateInstance() {
    return new TrelloTask();
  }
}

registerType(TrelloTask, new TrelloTaskType());