package com.fy.navi.service.adapter.aos;

import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.define.aos.FyTrafficUploadParameter;
import com.fy.navi.service.define.aos.RestrictedEndNumberParam;
import com.fy.navi.service.define.aos.RestrictedParam;

import java.util.Objects;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/2/5
 */
public class BlAosAdapter {
    private static final String AOS_PKG_NAME = Objects.requireNonNull(BlAosAdapter.class.getPackage()).getName();
    private static final String CLASS_API_NAME = "BlAosAdapterImpl";
    private final IBlAosApi mBlAosApi;


    private BlAosAdapter() {
        mBlAosApi = (IBlAosApi) AdapterConfig.getObject(AOS_PKG_NAME, CLASS_API_NAME);
    }

    public boolean initAosService() {
        return mBlAosApi.initBlAosService();
    }

    public boolean isInitAosService() {
        return mBlAosApi.isInit();
    }

    public void addRestrictedObserver(String key, QueryRestrictedObserver observer) {
        mBlAosApi.addRestrictedObserver(key, observer);
    }

    public void removeRestrictedObserver(String key) {
        mBlAosApi.removeRestrictedObserver(key);
    }

    public long queryRestrictedInfo(RestrictedParam restrictedParam) {
        return mBlAosApi.queryRestrictedInfo(restrictedParam);
    }

    public long queryRestrictedEndNumber(RestrictedEndNumberParam restrictedParam) {
        return mBlAosApi.queryRestrictedEndNumber(restrictedParam);
    }

    public long queryTrafficEventInfo(String eventId) {
        return mBlAosApi.queryTrafficEventInfo(eventId);
    }

    public long updateTrafficEvent(FyTrafficUploadParameter parameter) {
        return mBlAosApi.updateTrafficEvent(parameter);
    }

    public static BlAosAdapter getInstance() {
        return Helper.blAos;
    }

    public long queryTrafficPraiseInfo(String eventId) {
        return mBlAosApi.queryDynamicInfoEvent(eventId);
    }

    public void sendReqHolidayList(){
        mBlAosApi.sendReqHolidayList();
    }

    public static final class Helper {
        private static final BlAosAdapter blAos = new BlAosAdapter();
    }
}
