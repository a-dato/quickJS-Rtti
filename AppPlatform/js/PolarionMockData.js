export function CreateMockPolarionWorkItemsResponse() {
	return {
		data: [
			{
				type: 'workitems',
				id: 'a_dato/AD-101',
				revision: '1234',
				attributes: {
					created: '2026-04-01T08:30:00Z',
					description: {
						type: 'text/plain',
						value: 'Create the first mock work item import flow.'
					},
					dueDate: '2026-05-15',
					id: 'AD-101',
					initialEstimate: '2d',
					outlineNumber: '1.1',
					priority: '90.0',
					remainingEstimate: '1d',
					severity: 'normal',
					status: 'open',
					timeSpent: '1d',
					title: 'Mock import first work item',
					type: 'task',
					updated: '2026-04-20T10:15:00Z'
				},
				relationships: {
					assignee: {
						data: [
							{
								type: 'users',
								id: 'kees.vermeulen',
								revision: '1234'
							}
						],
						meta: {
							totalCount: 1
						}
					},
					author: {
						data: {
							type: 'users',
							id: 'melvin',
							revision: '1234'
						}
					},
					project: {
						data: {
							type: 'projects',
							id: 'a_dato',
							revision: '1234'
						}
					},
					linkedWorkItems: {
						data: [],
						meta: {
							totalCount: 0
						}
					}
				},
				links: {
					portal: 'https://mms.mithuntraining.com/polarion/redirect/project/a_dato/workitem?id=AD-101&revision=1234',
					self: 'https://mms.mithuntraining.com/polarion/rest/v1/projects/a_dato/workitems/AD-101?revision=1234'
				}
			},
			{
				type: 'workitems',
				id: 'a_dato/AD-102',
				revision: '1235',
				attributes: {
					created: '2026-04-02T09:00:00Z',
					description: {
						type: 'text/html',
						value: '<p>Map Polarion JSON:API work items to generic SourceItem objects.</p>'
					},
					dueDate: '2026-05-20',
					id: 'AD-102',
					initialEstimate: '5 1/2d',
					outlineNumber: '1.2',
					priority: '70.0',
					remainingEstimate: '3d',
					severity: 'blocker',
					status: 'in_progress',
					timeSpent: '2d',
					title: 'Map work items to SourceItem',
					type: 'requirement',
					updated: '2026-04-22T14:45:00Z'
				},
				relationships: {
					assignee: {
						data: [
							{
								type: 'users',
								id: 'melvin',
								revision: '1235'
							}
						],
						meta: {
							totalCount: 1
						}
					},
					author: {
						data: {
							type: 'users',
							id: 'kees.vermeulen',
							revision: '1235'
						}
					},
					project: {
						data: {
							type: 'projects',
							id: 'a_dato',
							revision: '1235'
						}
					},
					linkedWorkItems: {
						data: [
							{
								type: 'linkedworkitems',
								id: 'a_dato/AD-102/parent/a_dato/AD-101',
								revision: '1235'
							}
						],
						meta: {
							totalCount: 1
						}
					}
				},
				links: {
					portal: 'https://mms.mithuntraining.com/polarion/redirect/project/a_dato/workitem?id=AD-102&revision=1235',
					self: 'https://mms.mithuntraining.com/polarion/rest/v1/projects/a_dato/workitems/AD-102?revision=1235'
				}
			},
			{
				type: 'workitems',
				id: 'a_dato/AD-103',
				revision: '1236',
				attributes: {
					created: '2026-04-05T11:20:00Z',
					description: {
						type: 'text/plain',
						value: 'Use mock data while the Polarion REST server is unavailable.'
					},
					dueDate: '2026-05-24',
					id: 'AD-103',
					initialEstimate: '1d',
					outlineNumber: '1.3',
					priority: '50.0',
					remainingEstimate: '4h',
					severity: 'minor',
					status: 'open',
					timeSpent: '4h',
					title: 'Add mock response fallback',
					type: 'task',
					updated: '2026-04-23T09:10:00Z'
				},
				relationships: {
					assignee: {
						data: [
							{
								type: 'users',
								id: 'christopher',
								revision: '1236'
							}
						],
						meta: {
							totalCount: 1
						}
					},
					author: {
						data: {
							type: 'users',
							id: 'melvin',
							revision: '1236'
						}
					},
					project: {
						data: {
							type: 'projects',
							id: 'a_dato',
							revision: '1236'
						}
					},
					linkedWorkItems: {
						data: [
							{
								type: 'linkedworkitems',
								id: 'a_dato/AD-103/parent/a_dato/AD-102',
								revision: '1236'
							}
						],
						meta: {
							totalCount: 1
						}
					}
				},
				links: {
					portal: 'https://mms.mithuntraining.com/polarion/redirect/project/a_dato/workitem?id=AD-103&revision=1236',
					self: 'https://mms.mithuntraining.com/polarion/rest/v1/projects/a_dato/workitems/AD-103?revision=1236'
				}
			},
			{
				type: 'workitems',
				id: 'a_dato/AD-104',
				revision: '1237',
				attributes: {
					created: '2026-04-08T13:05:00Z',
					description: {
						type: 'text/html',
						value: '<p>Show imported source items in the Persons frame.</p>'
					},
					dueDate: '2026-05-31',
					id: 'AD-104',
					initialEstimate: '3d',
					outlineNumber: '1.4',
					priority: '80.0',
					remainingEstimate: '2d',
					severity: 'normal',
					status: 'review',
					timeSpent: '1d',
					title: 'Display imported persons from source items',
					type: 'feature',
					updated: '2026-04-25T16:30:00Z'
				},
				relationships: {
					assignee: {
						data: [
							{
								type: 'users',
								id: 'kees.vermeulen',
								revision: '1237'
							},
							{
								type: 'users',
								id: 'melvin',
								revision: '1237'
							}
						],
						meta: {
							totalCount: 2
						}
					},
					author: {
						data: {
							type: 'users',
							id: 'christopher',
							revision: '1237'
						}
					},
					project: {
						data: {
							type: 'projects',
							id: 'a_dato',
							revision: '1237'
						}
					},
					linkedWorkItems: {
						data: [],
						meta: {
							totalCount: 0
						}
					}
				},
				links: {
					portal: 'https://mms.mithuntraining.com/polarion/redirect/project/a_dato/workitem?id=AD-104&revision=1237',
					self: 'https://mms.mithuntraining.com/polarion/rest/v1/projects/a_dato/workitems/AD-104?revision=1237'
				}
			}
		],
		included: [],
		links: {
			first: 'https://mms.mithuntraining.com/polarion/rest/v1/all/workitems?page%5Bsize%5D=4&page%5Bnumber%5D=1',
			last: 'https://mms.mithuntraining.com/polarion/rest/v1/all/workitems?page%5Bsize%5D=4&page%5Bnumber%5D=1',
			self: 'https://mms.mithuntraining.com/polarion/rest/v1/all/workitems?page%5Bsize%5D=4&page%5Bnumber%5D=1',
			portal: 'https://mms.mithuntraining.com/polarion/#/project/a_dato/workitems'
		},
		meta: {
			totalCount: 4
		}
	};
}
