// Type-specific enrichments for the generic TypeMetadataProvider.
//
// Each extension implements:
//   AppliesTo(descriptor)      -> boolean, whether it wants to handle this descriptor
//   Extend(entry, provider)    -> object with extra JSON-safe keys to merge into the
//                                 descriptor entry (or null). May also adjust
//                                 entry.Properties in place.
//
// Keep all type-specific knowledge in this file so the provider stays generic.

// Card custom fields are defined per card type (ICardType.RequiredProperties),
// not on the card class itself like project/task custom fields. This extension
// moves the FIELD_xxxx properties from the flat card property list to the card
// type (instance from app.Storage['CardTypes']) that owns them.
class CardCustomFieldsExtension {
    AppliesTo(descriptor) {
        return String(descriptor?.StorageName ?? '') === 'Cards';
    }

    Extend(entry, provider) {
        let cardTypes = null;
        try {
            cardTypes = app.Storage['CardTypes'];
        } catch { /* storage not available */ }

        if(cardTypes == null)
            return null;

        const flatProperties = entry.Properties;
        const result = [];
        const ownedFieldNames = new Set();

        for(const cardType of provider.Items(cardTypes)) {
            if(cardType == null || cardType === 0)
                continue;

            const fields = [];
            const requiredProperties = cardType.RequiredProperties;
            if(requiredProperties != null && requiredProperties !== 0) {
                for(const cardTypeProperty of provider.Items(requiredProperties)) {
                    if(cardTypeProperty == null || cardTypeProperty === 0)
                        continue;

                    let key = null;
                    try {
                        key = String(cardTypeProperty.Key ?? '');
                    } catch { /* key not resolvable through the bridge */ }

                    // Built-in properties (Ettc, TimeSpent, ...) are already documented on the card itself
                    if(key == null || !key.startsWith('FIELD_'))
                        continue;

                    const flat = flatProperties.find(property => property.Name === key);

                    let typeName = null;
                    try {
                        typeName = typeof flat?.Type === 'function'
                            ? (flat.Type.name || null)
                            : (flat?.Type != null ? String(flat.Type) : null);
                    } catch { /* type not resolvable through the bridge */ }

                    const field = {
                        Name: key,
                        IsCustomField: true,
                        Type: typeName
                    };

                    let displayName = null;
                    try {
                        const value = cardTypeProperty.DisplayName;
                        if(value != null && String(value) !== '')
                            displayName = String(value);
                    } catch { /* display name not resolvable through the bridge */ }

                    if(displayName == null && flat?.DisplayName)
                        displayName = flat.DisplayName;
                    if(displayName != null)
                        field.DisplayName = displayName;

                    if(flat?.Description != null)
                        field.Description = flat.Description;

                    fields.push(field);
                    ownedFieldNames.add(key);
                }
            }

            let id = null;
            try {
                const rawId = cardType.ID;
                if(rawId != null) {
                    // Bridged CObject stringifies as "[object Object]"; use its own ToString
                    id = typeof rawId.ToString === 'function'
                        ? String(rawId.ToString())
                        : String(rawId);
                }
            } catch { /* id not resolvable through the bridge */ }

            result.push({
                Name: String(cardType.Name ?? ''),
                ID: id,
                Properties: fields
            });
        }

        // Fields claimed by a card type no longer belong in the flat card property list
        for(let i = flatProperties.length - 1; i >= 0; i--) {
            if(ownedFieldNames.has(flatProperties[i].Name))
                flatProperties.splice(i, 1);
        }

        return { CardTypes: result };
    }
}

export const TYPE_METADATA_EXTENSIONS = [
    new CardCustomFieldsExtension()
];
