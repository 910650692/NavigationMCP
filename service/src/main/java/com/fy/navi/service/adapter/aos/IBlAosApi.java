package com.fy.navi.service.adapter.aos;

import com.fy.navi.service.define.aos.FyTrafficUploadParameter;
import com.fy.navi.service.define.aos.RestrictedParam;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/2/5
 */
public interface IBlAosApi {
    boolean initBlAosService();

    boolean isInit();

    void addRestrictedObserver(String key, QueryRestrictedObserver observer);

    void removeRestrictedObserver(String key);

    /**
     * 查询限行信息
     * <1>查询指定城市限行信息</>
     * <2>查询限行城市列表</>
     * <3>查询指定城市指定规则的限行信息</>
     *
     * @param restrictedParam 查询参数
     */
    long queryRestrictedInfo(RestrictedParam restrictedParam);

    /***
     * 查询交通事件详情信息
     * @param poiId
     * @return
     */
    long queryTrafficEventInfo(String poiId);

    /**
     * 更新交通事件
     */
    long updateTrafficEvent(FyTrafficUploadParameter parameter);

    /***
     * 查询交通事件 ”踩“和”赞“
     * @return
     */
    long queryDynamicInfoEvent(String trafficEventId);

    /***
     * 查询是否是节假日
     * @return
     */
    void sendReqHolidayList();
}
