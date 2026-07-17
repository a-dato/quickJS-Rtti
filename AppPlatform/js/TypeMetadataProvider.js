import PROJECT_INFO from '../ProjectInfo.js';
import CARD_INFO from '../CardInfo.js';
import RESOURCE_CLASS_INFO from '../ResourceClassInfo.js';
import RESOURCE_INFO from '../ResourceInfo.js';
import TIME_TRACK_DETAIL_INFO from '../TimeTrackDetailInfo.js';
import TASK_INFO from '../TaskInfo.js';
import { TYPE_METADATA_EXTENSIONS } from './TypeMetadataExtensions.js';

const TYPE_INFO = {
    ...PROJECT_INFO,
    ...CARD_INFO,
    ...RESOURCE_CLASS_INFO,
    ...RESOURCE_INFO,
    ...TIME_TRACK_DETAIL_INFO,
    ...TASK_INFO
};

export class TypeMetadataProvider {
    TypeInfo() {
        return TYPE_INFO;
    }

    ProjectInfo() {
        return this.TypeInfo();
    }

    Types() {
        return this.GetAvailableTypes();
    }

    GetAvailableTypes() {
        const types = app.Config.Types;
        if(types == null)
            throw new Error('app.Config does not expose its registered types.');

        const result = [];
        for(const type of this.Items(types)) {
            if(type == null || type === 0)
                continue;

            const descriptor = app.Config.TypeDescriptor(type);

            result.push({
                Name: type.name ?? type.Name,
                Type: type,
                Descriptor: descriptor,
                DescriptorName: descriptor?.Name ?? descriptor?.TypeName ?? descriptor?.ClassName,
                Properties: this.GetTypeProperties(type)
            });
        }

        return result;
    }

    GetTypeDescriptors() {
        const descriptors = app.Config.TypeDescriptors;
        if(descriptors == null)
            throw new Error('app.Config does not expose its registered type descriptors.');

        const result = [];
        for(const descriptor of this.Items(descriptors)) {
            if(descriptor == null || descriptor === 0)
                continue;

            let type = null;
            if(typeof descriptor.GetType === 'function')
                type = descriptor.GetType();

            const properties = [];
            if(type != null) {
                for(const property of this.GetTypeProperties(type))
                    properties.push(property);
            } else if(descriptor.PropertyDescriptor != null) {
                for(const name of Object.keys(descriptor.PropertyDescriptor)) {
                    const propertyDescriptor = descriptor.PropertyDescriptor[name];
                    const description = this.GetPropertyDescription(descriptor.ClassName ?? descriptor.TypeName ?? descriptor.FullName, name);
                    const entry = {
                        Name: name,
                        IsCustomField: String(name).startsWith('FIELD_'),
                        Type: propertyDescriptor?.Type ?? null,
                        PropertyDescriptor: propertyDescriptor
                    };
                    if(description != null)
                        entry.Description = description;

                    properties.push(entry);
                }
            }

            const entry = {
                ClassName: descriptor.ClassName,
                FullName: descriptor.FullName,
                StorageName: descriptor.StorageName,
                Type: type,
                Descriptor: descriptor,
                Constructor: this.GetConstructor(type),
                Interface: this.GetInterface(descriptor.TypeInterface),
                SupportedInterfaces: this.GetSupportedInterfaces(descriptor),
                Properties: properties
            };

            this.ApplyExtensions(descriptor, entry);

            result.push(entry);
        }

        return result;
    }

    // Runs the registered type metadata extensions (see TypeMetadataExtensions.js)
    // against a descriptor entry. Extra keys returned by an extension are collected
    // in entry.Extensions and merged verbatim into the serialized JSON.
    ApplyExtensions(descriptor, entry) {
        for(const extension of TYPE_METADATA_EXTENSIONS) {
            if(!extension.AppliesTo(descriptor))
                continue;

            const extra = extension.Extend(entry, this);
            if(extra != null)
                entry.Extensions = { ...(entry.Extensions ?? {}), ...extra };
        }
    }

    GetSupportedInterfaces(descriptor) {
        const supportedInterfaces = [];
        const sourceInterfaces = descriptor?.SupportedInterfaces;
        if(sourceInterfaces == null || sourceInterfaces === 0)
            return supportedInterfaces;

        for(const supportedInterface of this.Items(sourceInterfaces)) {
            if(supportedInterface == null || supportedInterface === 0)
                continue;

            supportedInterfaces.push(this.GetInterface(supportedInterface));
        }

        return supportedInterfaces;
    }

    GetInterface(sourceInterface) {
        if(sourceInterface == null || sourceInterface === 0)
            return null;

        const methods = [];
        if(sourceInterface.Methods != null && sourceInterface.Methods !== 0) {
            for(const method of this.Items(sourceInterface.Methods)) {
                if(method == null || method === 0)
                    continue;

                methods.push({
                    Name: method.Name ?? null,
                    Description: method.Description ?? null,
                    UsagePattern: method.UsagePattern ?? null
                });
            }
        }

        const behaviorName = sourceInterface.BehaviorName ?? null;
        const behaviorDescription = sourceInterface.BehaviorDescription ?? null;
        const behaviorUseWhen = sourceInterface.BehaviorUseWhen ?? null;
        const behavior = behaviorName == null && behaviorDescription == null && behaviorUseWhen == null
            ? null
            : {
                Name: behaviorName,
                Description: behaviorDescription,
                UseWhen: behaviorUseWhen
            };

        const result = {
            Name: sourceInterface.Name ?? this.GetTypeName(sourceInterface.InterfaceType),
            Type: sourceInterface.InterfaceType ?? null,
            TypeName: this.GetTypeName(sourceInterface.InterfaceType) ?? sourceInterface.Name ?? null,
            Behavior: behavior,
            Methods: methods
        };

        return result;
    }

    GetTypeName(type) {
        if(type == null || type === 0)
            return null;

        try {
            return type.Name ?? type.name ?? String(type);
        } catch {
            return null;
        }
    }

    GetConstructor(type) {
        if(type == null || type === 0)
            return null;

        const factory = app.Factory;
        if(factory == null || typeof factory.GetConstructorParameterCount !== 'function')
            return null;

        try {
            const parameterCount = factory.GetConstructorParameterCount(type);
            if(parameterCount < 0)
                return null;

            const parameters = [];
            for(let i = 0; i < parameterCount; i++)
                parameters.push({ Name: `Param${i}`, Type: 'CObject' });

            return {
                ParameterCount: parameterCount,
                Parameters: parameters
            };
        } catch {
            return null;
        }
    }

    GetPropertyDescription(type, propertyName) {
        const typeInfo = this.TypeInfo();
        const names = [
            this.GetTypeName(type),
            type?.ClassName,
            type?.TypeName,
            type?.FullName,
            String(type ?? '')
        ];

        for(const name of names) {
            if(name == null || name === '')
                continue;

            const normalizedName = String(name).startsWith('I') && String(name).length > 1
                ? String(name).substring(1)
                : String(name);
            const descriptions = typeInfo[String(name)] ?? typeInfo[normalizedName];
            const description = descriptions?.[propertyName];
            if(description != null)
                return description;
        }

        return null;
    }

    GetTypeProperties(type) {
        const properties = [];
        const sourceProperties = app.Config.GetProperties(type);

        for(const property of this.Items(sourceProperties)) {
            const name = String(property.Name);
            const isCustomField = name.startsWith('FIELD_');

            const entry = {
                Name: name,
                IsCustomField: isCustomField,
                Type: typeof property.GetType === 'function'
                    ? property.GetType()
                    : property.Type,
                PropertyInfo: property,
            };

            const description = this.GetPropertyDescription(type, name);
            if(description != null)
                entry.Description = description;

            if(isCustomField) {
                try {
                    const value = app.Config.PropertyDisplayName(type, name);
                    if(value != null && String(value) !== '' && String(value) !== name)
                        entry.DisplayName = String(value);
                } catch {  }
            }

            properties.push(entry);
        }

        return properties;
    }

    GetAvailableTypesAsJSON() {
        const types = [];

        for(const type of this.Types()) {
            const properties = [];

            for(const prop of type.Properties) {
                let typeName = null;
                try {
                    typeName = typeof prop.Type === 'function'
                        ? (prop.Type.name || null)
                        : (prop.Type != null ? String(prop.Type) : null);
                } catch { /* type not resolvable through the bridge */ }

                const entry = { Name: prop.Name, Type: typeName };
                if(prop.Description != null)
                    entry.Description = prop.Description;
                if(prop.IsCustomField) {
                    entry.IsCustomField = true;
                    if(prop.DisplayName)
                        entry.DisplayName = prop.DisplayName;
                }
                properties.push(entry);
            }

            types.push({
                Name: type.Name,
                DescriptorName: type.DescriptorName ?? null,
                Properties: properties
            });
        }

        return JSON.stringify(types, null, 2);
    }

    TypeDescriptorsAsJSON() {
        const descriptors = [];

        for(const descriptor of this.GetTypeDescriptors()) {
            const properties = [];

            for(const prop of descriptor.Properties) {
                let typeName = null;
                try {
                    typeName = typeof prop.Type === 'function'
                        ? (prop.Type.name || null)
                        : (prop.Type != null ? String(prop.Type) : null);
                } catch { /* type not resolvable through the bridge */ }

                const entry = { Name: prop.Name, Type: typeName };
                if(prop.Description != null)
                    entry.Description = prop.Description;
                if(prop.IsCustomField) {
                    entry.IsCustomField = true;
                    if(prop.DisplayName)
                        entry.DisplayName = prop.DisplayName;
                }
                properties.push(entry);
            }

            descriptors.push({
                ClassName: descriptor.ClassName ?? null,
                FullName: descriptor.FullName ?? null,
                StorageName: descriptor.StorageName ?? null,
                TypeName: descriptor.Type?.Name ?? descriptor.Type?.name ?? null,
                ...(descriptor.Extensions ?? {}),
                Constructor: descriptor.Constructor ?? undefined,
                Interface: descriptor.Interface == null
                    ? null
                    : {
                        Name: descriptor.Interface.Name ?? null,
                        TypeName: descriptor.Interface.TypeName ?? null,
                        Behavior: descriptor.Interface.Behavior,
                        Methods: descriptor.Interface.Methods
                    },
                SupportedInterfaces: descriptor.SupportedInterfaces.map(supportedInterface => ({
                    Name: supportedInterface.Name ?? null,
                    TypeName: supportedInterface.TypeName ?? null,
                    Behavior: supportedInterface.Behavior,
                    Methods: supportedInterface.Methods
                })),
                Properties: properties
            });
        }

        return JSON.stringify(descriptors, null, 2);
    }

    // Iterates a bridged Delphi list or plain JS array, whichever access pattern it supports.
    *Items(list) {
        let count = typeof list.Count === 'function' ? list.Count() : list.Count;
        if(typeof count !== 'number')
            count = list.length;

        if(typeof count === 'number' && count >= 0) {
            for(let i = 0; i < count; i++)
                yield list[i];
            return;
        }

        if(typeof list.GetEnumerator === 'function') {
            const enumerator = list.GetEnumerator();
            while(enumerator.MoveNext())
                yield enumerator.Current;
            return;
        }

        if(typeof list[Symbol.iterator] === 'function') {
            yield* list;
            return;
        }

        console.log('Items: no way to iterate this list (no Count, length, GetEnumerator or iterator)');
    }
}