package com.sgm.navi.service.define.aos;

import lombok.Getter;
import lombok.Setter;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/2/5
 */
@Getter
@Setter
public class RestrictedParam {
    /*** 请求限行策略 **/
    private int restrict_type = 0;
    /*** 车牌号 **/
    private String plate = "";
    /*** 指定规则 **/
    private String ruleids = "";
    /*** 城市Code **/
    private String adcodes = "";

    public RestrictedParam() {
    }

    public RestrictedParam(int restrict_type, String plate, String ruleids, String adcodes) {
        this.restrict_type = restrict_type;
        this.plate = plate;
        this.ruleids = ruleids;
        this.adcodes = adcodes;
    }
}
