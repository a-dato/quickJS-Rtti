import {SourceItem, SourceDataPerson, SourceDataTask} from './SourceItem.js';
import {CreateMockPolarionWorkItemsResponse} from './PolarionMockData.js';

const DEFAULT_BASE_URL = 'https://mms.mithuntraining.com/polarion/rest/v1';
const DEFAULT_TOKEN = 'eyJraWQiOiI3NjU4YTMxYi1hYzExMDAwMi0wMWRjNzE3My1mNTBhOGQzMCIsInR5cCI6IkpXVCIsImFsZyI6IlJTMjU2In0.eyJzdWIiOiJrZWVzLnZlcm1ldWxlbkBhLWRhdG8ubmV0IiwiaWQiOiJkMzUyOTNiMS1hYzExMDAwMi0xNTE0YzQxNy1jZDY1NGE4OCIsImV4cCI6MTc4NTE0Mjg5NywiaWF0IjoxNzc3MzY2ODk3fQ.Ytb7eHO10h1qQT_RHCi61lxpTAllJINMoXq4-2NLr7XvfchsBQBSEbwbkntuYA65zZsE98B57OPrk2bdoSLDpEw4e3FTy190MLOX9KKDXDJjG3UF_Vk1T2ec2xdYB3KK69sbGvcHfC0eya6D9wL8PvrE9sibDKKEB83ZFPclWg42l7O_zeDFiRhb1k7AD8iZX3QJaJQBp-HIPlkDb1CfuZVuvOzv2RKztk6buQdF0GDR852W6zcHM5uLo8hJGA4VZt1SkL9O80rfno6Sd7HL9Yx6vQkwDweBrIiT8CG9PlCAXuHRPGKHAu6YuU1D1X3j2GPdR3nTFBauej31Rm2v7w';

export class PolarionFetcher {
	constructor(token = DEFAULT_TOKEN, baseUrl = DEFAULT_BASE_URL) {
		this.Token = token;
		this.BaseUrl = baseUrl;
	}

	get Headers() {
		const headers = {
			'Accept': 'application/json',
			'X-Polarion-Client-Type': 'SwaggerUI'
		};

		if(this.Token != null && this.Token !== '') {
			headers.Authorization = `Bearer ${this.Token}`;
		}

		return headers;
	}

	async FetchGET(resource) {
		const url = `${this.BaseUrl}${resource}`;
		console.log(`Polarion GET ${url}`);

		return fetch(url, {
			method: 'GET',
			headers: this.Headers
		});
	}

	async WorkItems(project, filter) {
		console.log('Polarion work items loading');

		//return CreateMockPolarionWorkItemsResponse().data;

		const response = await this.FetchGET('/all/workitems?page[size]=150&fields[workitems]=@all');
		if (!response.ok) {
			throw new Error(`HTTP error! Status: ${response.status}`);
		}

		const data = await response.json();
		return data.data ?? data.items ?? data._embedded?.elements ?? [];
	}
}

export class PolarionWorkItemSource {
	constructor(fetcher = null, typeName = 'Person') {
		this.Fetcher = fetcher ?? new PolarionFetcher();
		this.TypeName = typeName;
	}

	MapItem(item) {
		const attributes = item.attributes ?? item;
		const id = attributes.id ?? item.id ?? item.key ?? item.uri;
		const description = attributes.title
			?? item.title
			?? attributes.name
			?? item.name
			?? id;
		const sourceData = this.TypeName === 'Person'
			? new SourceDataPerson(description, attributes.age ?? 0)
			: new SourceDataTask(id, description);

		return new SourceItem(this.TypeName, sourceData);
	}

	async Items(filter) {
		const items = await this.Fetcher.WorkItems(null, filter);
		return items.map((item) => this.MapItem(item));
	}
}
