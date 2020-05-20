import { LitElement, html, customElement, property } from 'lit-element';
import * as $ from 'jquery';
import 'bootstrap';
import 'bootstrap-select';
import 'bootstrap-select/dist/css/bootstrap-select.min.css';

type Item = OptGroup | Option

interface Option {
    type: string;
    value: string;
}

interface OptGroup {
    type: string;
    label: string;
    value: string[];
}

function isOption(item: Item): item is Option {
    return item.type == 'option';
}

function isOptGroup(item: Item): item is OptGroup {
    return item.type == 'optgrp';
}

@customElement('nice-select')
export class NiceSelect extends LitElement {
    
    private _node?: JQuery<Element>;
    private _value: string = '';

    @property({ type : Array })
    items = [];

    @property({ type : Boolean, reflect: true })
    searchable = false;

    @property({ type : Boolean, reflect: true })
    nullable = false;

    @property({ type : Boolean, reflect: true })
    disabled = false;

    @property({ type : String, reflect: true })
    public get value(): string | null {
        return this._value;
    }
    public set value(value: string | null) {
        let val = value ?? '';
        if(val == this.value)
            return;
        this._value = val;
        this.updateValue();
    }

    private updateValue() {
        this._node?.selectpicker('val', this._value ?? '');
    }

    firstUpdated() {
        this._node = $(this).find(".selectpicker");
        this._node.on('changed.bs.select', (e, clickedIndex, isSelected, previousValue) => {
            if(clickedIndex) {
                this._value = (this._node?.val() as string|null) ?? '';
                this.dispatchEvent(new CustomEvent('select-user-update', {
                    bubbles: true,
                    detail: {
                        value: this.nullable && clickedIndex == 1 ? null : this._value,
                        index: clickedIndex
                    }
                }));
            }
        });
    }

    updated() {
        this._node?.selectpicker('refresh');
        this.updateValue();
    }

    createRenderRoot() {
        return this;
    }

    private renderItem(item: Item) {
        if(isOptGroup(item))
            return this.renderOptGroup(item.label, item.value);
        else if(isOption(item))
            return this.renderOption(item.value);
    }

    private renderOptGroup(name: string, options: string[]) {
        return html`
            <optgroup label = ${name}>
                ${options.map(opt => this.renderOption(opt, name))}
            </optgroup>
        `;
    }

    private renderOption(opt: string, keyword?: string) {
        return html`
            <option data-tokens = ${keyword} >
                ${opt}
            </option>
        `;
    }

    render() {
        return html`
            <select
                class="selectpicker form-control"
                ?disabled = ${this.disabled}
                data-live-search = ${this.searchable ? "true" : "false"}
                title = "Nichts ausgewählt"
                data-none-results-text = "Keine Treffer">
                ${this.nullable ? html`<option></option>` : null}
                ${this.items.map(item => this.renderItem(item))}
            </select>
        `;
    }
}