declare var stripe: any;
declare var elements: any;
export function getArrayLength(arr: any[]): number;
export const maxInterval: 12;
declare const helloWorld: RegExp;
declare const pi: number;
export default pi;
declare namespace getArrayLength {
  declare const maxInterval: 12;
}
export = getArrayLength;
export type ArrayMetadata = {
  length: number;
  firstObject: any | undefined;
};
export function getArrayMetadata(arr: any[]): ArrayMetadata;
export type ArrayMetadata<ArrType> = {
  length: number;
  firstObject: ArrType | undefined;
};
export function getArrayMetadata<ArrType>(
  arr: ArrType[]
): ArrayMetadata<ArrType>;
