package com.sgm.navi.service.logicpaket.aos;

import com.android.utils.ConvertUtils;
import com.android.utils.TimeUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.service.adapter.aos.BlAosAdapter;
import com.sgm.navi.service.adapter.aos.QueryRestrictedObserver;
import com.sgm.navi.service.define.aos.FyCriticism;
import com.sgm.navi.service.define.aos.FyGTraEventDetail;
import com.sgm.navi.service.define.aos.FyTrafficUploadParameter;
import com.sgm.navi.service.define.aos.RestrictedEndNumberParam;
import com.sgm.navi.service.define.aos.RestrictedParam;
import com.sgm.navi.service.define.aos.TrafficRestrictResponseParam;
import com.sgm.navi.service.define.bean.PreviewParams;
import com.sgm.navi.service.define.layer.refix.DynamicLevelMode;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.route.RouteRestrictionParam;
import com.sgm.navi.service.logicpaket.map.IMapPackageCallback;
import com.sgm.navi.service.logicpaket.map.MapPackage;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/2/6
 */
public class AosRestrictedPackage implements QueryRestrictedObserver {
    private static final String TAG = "AosRestrictedPackage";
    private final BlAosAdapter mBlAosAdapter;
    private final MapPackage mMapPackage;
    private final Hashtable<String, IAosRestrictedObserver> restrictedObserverList;
    private final Hashtable<String, List<IMapPackageCallback>> callbackTables = new Hashtable<>();

    private AosRestrictedPackage() {
        mBlAosAdapter = BlAosAdapter.getInstance();
        mMapPackage = MapPackage.getInstance();
        restrictedObserverList = new Hashtable<>();
        mBlAosAdapter.addRestrictedObserver("AosRestrictedPackage", this);
    }

    public boolean initAosService() {
        return mBlAosAdapter.initAosService();
    }

    public boolean isInitAosService() {
        return mBlAosAdapter.isInitAosService();
    }

    public void addRestrictedObserver(String key, IAosRestrictedObserver observer) {
        restrictedObserverList.put(key, observer);
    }

    public void removeRestrictedObserver(String key) {
        restrictedObserverList.remove(key);
    }

    public long queryRestrictedInfo(RestrictedParam restrictedParam) {
        return mBlAosAdapter.queryRestrictedInfo(restrictedParam);
    }

    public long queryRestrictedEndNumber(RestrictedEndNumberParam restrictedParam) {
        return mBlAosAdapter.queryRestrictedEndNumber(restrictedParam);
    }

    public long queryTrafficEventInfo(String eventId) {
        return mBlAosAdapter.queryTrafficEventInfo(eventId);
    }

    public long queryTrafficPraiseInfo(String eventId) {
        return mBlAosAdapter.queryTrafficPraiseInfo(eventId);
    }

    public long updateTrafficEvent(FyTrafficUploadParameter parameter) {
        return mBlAosAdapter.updateTrafficEvent(parameter);
    }

    public void sendReqHolidayList(){
        mBlAosAdapter.sendReqHolidayList();
    }

    public static AosRestrictedPackage getInstance() {
        return Helper.aosPackage;
    }

    /**
     * 限行页面路线全览
     * @param mapTypeId 屏幕ID
     */
    public void showRestrictedAreaPreview(final MapType mapTypeId, final RouteRestrictionParam param, final int index) {
        List<PreviewParams.PointD> points = new ArrayList<>();
        if (!ConvertUtils.isEmpty(param) && !ConvertUtils.isEmpty(param.getMRestrictedArea())
                && !ConvertUtils.isEmpty(param.getMRestrictedArea().getMPointList())
                && index < param.getMRestrictedArea().getMPointList().size() ) {
            points = param.getMRestrictedArea().getMPointList().get(index);
        }
        mMapPackage.showPreview(mapTypeId, false, 1200, 210, 600, 140, points, DynamicLevelMode.DYNAMIC_LEVEL_GUIDE);
    }

    @Override
    public void onDrawRestrictionAndDetails(RouteRestrictionParam param) {
        synchronized (restrictedObserverList) {
            for (IAosRestrictedObserver observer : restrictedObserverList.values()) {
                observer.queryLimitResult(param);
            }
        }
    }

    @Override
    public void onTrafficRestrict(TrafficRestrictResponseParam trafficRestrictResponseParam) {
        synchronized (restrictedObserverList) {
            for (IAosRestrictedObserver observer : restrictedObserverList.values()) {
                observer.queryLimitEndNumberResult(trafficRestrictResponseParam);
            }
        }
    }

    @Override
    public void onTrafficQueryDetail(FyGTraEventDetail gTraEventDetail) {
        QueryRestrictedObserver.super.onTrafficQueryDetail(gTraEventDetail);
        synchronized (restrictedObserverList) {
            for (IAosRestrictedObserver observer : restrictedObserverList.values()) {
                observer.queryTrafficEventDetailResult(gTraEventDetail);
            }
        }
    }

    @Override
    public void onTrafficUploadFinished(boolean isSuccess) {
        QueryRestrictedObserver.super.onTrafficUploadFinished(isSuccess);
        synchronized (restrictedObserverList) {
            for (IAosRestrictedObserver observer : restrictedObserverList.values()) {
                observer.onTrafficUploadFinished(isSuccess);
            }
        }
    }

    @Override
    public void onDynamicPraiseQueryFinished(FyCriticism fyCriticism) {
        QueryRestrictedObserver.super.onDynamicPraiseQueryFinished(fyCriticism);
        synchronized (restrictedObserverList) {
            for (IAosRestrictedObserver observer : restrictedObserverList.values()) {
                observer.onDynamicPraiseQueryFinished(fyCriticism);
            }
        }
    }

    @Override
    public void onRecvAck(ArrayList<String> data) {
        boolean isHoliday = false;
        if(!ConvertUtils.isEmpty(data)){
            //判断当前日期是否是节假日
            final String currentDay = TimeUtils.convertYMD();
            Logger.d("onRecvAck---"+currentDay);

            for (int i = 0; i < data.size(); i++) {
                String holiday = data.get(i).replace("-","");
                Logger.d("onRecvAck+++"+holiday);
                if(ConvertUtils.equals(currentDay,holiday)){
                    isHoliday = true;
                    break;
                }
            }
        }

        synchronized (restrictedObserverList) {
            for (IAosRestrictedObserver observer : restrictedObserverList.values()) {
                observer.isHoliday(isHoliday);
            }
        }

    }

    private static final class Helper {
        private static final AosRestrictedPackage aosPackage = new AosRestrictedPackage();
    }
}
