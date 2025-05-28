package com.fy.navi.service.define.signal;

public class RoadConditionGroupSecond {
    private Integer lngthDynInfmAryOfNavRut;//导航路段信息数组长度
    private Integer estimRemnDistn;//导航路段剩余里程
    private Integer estimRemnTim;//导航剩余时长
    private Integer dataInv;//数据有效性

    public Integer getLngthDynInfmAryOfNavRut() {
        return lngthDynInfmAryOfNavRut;
    }

    public void setLngthDynInfmAryOfNavRut(Integer lngthDynInfmAryOfNavRut) {
        this.lngthDynInfmAryOfNavRut = lngthDynInfmAryOfNavRut;
    }

    public Integer getEstimRemnDistn() {
        return estimRemnDistn;
    }

    public void setEstimRemnDistn(Integer estimRemnDistn) {
        this.estimRemnDistn = estimRemnDistn;
    }

    public Integer getEstimRemnTim() {
        return estimRemnTim;
    }

    public void setEstimRemnTim(Integer estimRemnTim) {
        this.estimRemnTim = estimRemnTim;
    }

    public Integer getDataInv() {
        return dataInv;
    }

    public void setDataInv(Integer dataInv) {
        this.dataInv = dataInv;
    }

    @Override
    public String toString() {
        return "RoadConditionGroupSecond{" +
                "lngthDynInfmAryOfNavRut=" + lngthDynInfmAryOfNavRut +
                ", estimRemnDistn=" + estimRemnDistn +
                ", estimRemnTim=" + estimRemnTim +
                ", dataInv=" + dataInv +
                '}';
    }
}
