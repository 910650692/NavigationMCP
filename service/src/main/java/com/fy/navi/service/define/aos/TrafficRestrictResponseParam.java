package com.fy.navi.service.define.aos;

import lombok.Getter;
import lombok.Setter;

/**
 * @Description TODO
 * @Author liuchang
 * @date 2025/6/3
 */
@Getter
@Setter
public class TrafficRestrictResponseParam {
    /*** 请求id **/
    private long taskId;
    /*** 限行尾号 **/
    private String plateNo;
    /*** 限行信息 **/
    private String info;
    /***  是否长期限行城市(0:否,1:是) ,如：0 **/
    private int cityFlag;
    /***  是否限行(根据车牌号计算 0:不限行,1:限行) ,如：0 **/
    private int restrictFlag;

    public TrafficRestrictResponseParam() {
    }

}
