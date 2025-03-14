package com.fy.navi.service.logicpaket.aos;

import com.fy.navi.service.adapter.aos.BlAosAdapter;
import com.fy.navi.service.adapter.aos.QueryRestrictedObserver;
import com.fy.navi.service.define.aos.FyCriticism;
import com.fy.navi.service.define.aos.FyGTraEventDetail;
import com.fy.navi.service.define.aos.FyTrafficUploadParameter;
import com.fy.navi.service.define.aos.RestrictedArea;
import com.fy.navi.service.define.aos.RestrictedParam;
import com.fy.navi.service.define.route.RouteRestrictionParam;
import com.fy.navi.service.logicpaket.map.IMapPackageCallback;

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
    private final Hashtable<String, IAosRestrictedObserver> restrictedObserverList;
    private final Hashtable<String, List<IMapPackageCallback>> callbackTables = new Hashtable<>();

    private AosRestrictedPackage() {
        mBlAosAdapter = BlAosAdapter.getInstance();
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

    public long queryTrafficEventInfo(String eventId) {
        return mBlAosAdapter.queryTrafficEventInfo(eventId);
    }

    public long queryTrafficPraiseInfo(String eventId) {
        return mBlAosAdapter.queryTrafficPraiseInfo(eventId);
    }

    public long updateTrafficEvent(FyTrafficUploadParameter parameter) {
        return mBlAosAdapter.updateTrafficEvent(parameter);
    }

    public static AosRestrictedPackage getInstance() {
        return Helper.aosPackage;
    }

    @Override
    public void onDrawRestrictionAndDetails(RouteRestrictionParam param) {
        for (IAosRestrictedObserver observer : restrictedObserverList.values()) {
            observer.queryLimitResult(param);
        }
    }

    @Override
    public void onTrafficQueryDetail(FyGTraEventDetail gTraEventDetail) {
        QueryRestrictedObserver.super.onTrafficQueryDetail(gTraEventDetail);
        for (IAosRestrictedObserver observer : restrictedObserverList.values()) {
            observer.queryTrafficEventDetailResult(gTraEventDetail);
        }
    }

    @Override
    public void onTrafficUploadFinished(boolean isSuccess) {
        QueryRestrictedObserver.super.onTrafficUploadFinished(isSuccess);
        for (IAosRestrictedObserver observer : restrictedObserverList.values()) {
            observer.onTrafficUploadFinished(isSuccess);
        }
    }

    @Override
    public void onDynamicPraiseQueryFinished(FyCriticism fyCriticism) {
        QueryRestrictedObserver.super.onDynamicPraiseQueryFinished(fyCriticism);
        for (IAosRestrictedObserver observer : restrictedObserverList.values()) {
            observer.onDynamicPraiseQueryFinished(fyCriticism);
        }
    }

    private static final class Helper {
        private static final AosRestrictedPackage aosPackage = new AosRestrictedPackage();
    }
}
