package com.fy.navi.service.define.signal;

public class RoadConditionGroupFirst {
    private Integer indxOfDynmInftAryNavRut; // 导航路段信息索引号
    private Integer estimDistnCorpToIndxRut; // 对应索引号的预估路段长度
    private Integer estimTimCorpToIndxRut; // 通过路段的预估时长
    private Integer estimRodCndtnCorpToIndxRut; // 对应索引号的道路状况

    public Integer getIndxOfDynmInftAryNavRut() {
        return indxOfDynmInftAryNavRut;
    }

    public void setIndxOfDynmInftAryNavRut(Integer indxOfDynmInftAryNavRut) {
        this.indxOfDynmInftAryNavRut = indxOfDynmInftAryNavRut;
    }

    public Integer getEstimDistnCorpToIndxRut() {
        return estimDistnCorpToIndxRut;
    }

    public void setEstimDistnCorpToIndxRut(Integer estimDistnCorpToIndxRut) {
        this.estimDistnCorpToIndxRut = estimDistnCorpToIndxRut;
    }

    public Integer getEstimTimCorpToIndxRut() {
        return estimTimCorpToIndxRut;
    }

    public void setEstimTimCorpToIndxRut(Integer estimTimCorpToIndxRut) {
        this.estimTimCorpToIndxRut = estimTimCorpToIndxRut;
    }

    public Integer getEstimRodCndtnCorpToIndxRut() {
        return estimRodCndtnCorpToIndxRut;
    }

    public void setEstimRodCndtnCorpToIndxRut(Integer estimRodCndtnCorpToIndxRut) {
        this.estimRodCndtnCorpToIndxRut = estimRodCndtnCorpToIndxRut;
    }

    @Override
    public String toString() {
        return "RoadConditionGroupFirst{" +
                "indxOfDynmInftAryNavRut=" + indxOfDynmInftAryNavRut +
                ", estimDistnCorpToIndxRut=" + estimDistnCorpToIndxRut +
                ", estimTimCorpToIndxRut=" + estimTimCorpToIndxRut +
                ", estimRodCndtnCorpToIndxRut=" + estimRodCndtnCorpToIndxRut +
                '}';
    }
}
