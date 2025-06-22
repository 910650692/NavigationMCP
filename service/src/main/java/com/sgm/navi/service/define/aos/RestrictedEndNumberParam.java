package com.sgm.navi.service.define.aos;

import lombok.Getter;
import lombok.Setter;

/**
 * @Description TODO
 * @Author liuchang
 * @date 2025/6/3
 */
@Getter
@Setter
public class RestrictedEndNumberParam {
    /*** 车牌号 **/
    private String plate = "";
    /*** 城市Code **/
    private long adcodes;

    public RestrictedEndNumberParam() {
    }

    public RestrictedEndNumberParam(String plate, long adcodes) {
        this.plate = plate;
        this.adcodes = adcodes;
    }
}
