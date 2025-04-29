package com.fy.navi.service.logicpaket.aos;

import com.android.utils.ConvertUtils;
import com.fy.navi.service.adapter.aos.BlAosAdapter;
import com.fy.navi.service.adapter.aos.QueryRestrictedObserver;
import com.fy.navi.service.adapter.map.MapAdapter;
import com.fy.navi.service.define.aos.FyCriticism;
import com.fy.navi.service.define.aos.FyGTraEventDetail;
import com.fy.navi.service.define.aos.FyTrafficUploadParameter;
import com.fy.navi.service.define.aos.RestrictedArea;
import com.fy.navi.service.define.aos.RestrictedParam;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.map.MapType;
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
    private final MapAdapter mMapAdapter;
    private final Hashtable<String, IAosRestrictedObserver> restrictedObserverList;
    private final Hashtable<String, List<IMapPackageCallback>> callbackTables = new Hashtable<>();

    private AosRestrictedPackage() {
        mBlAosAdapter = BlAosAdapter.getInstance();
        mMapAdapter = MapAdapter.getInstance();
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

    /**
     * 限行页面路线全览
     * @param mapTypeId 屏幕ID
     */
    public void showRestrictedAreaPreview(final MapType mapTypeId, final RouteRestrictionParam param, final int index) {
        final PreviewParams previewParams = new PreviewParams();
        previewParams.setRouteLine(false);
        previewParams.setbUseRect(false);
        if (!ConvertUtils.isEmpty(param) && !ConvertUtils.isEmpty(param.getMRestrictedArea())
                && !ConvertUtils.isEmpty(param.getMRestrictedArea().getMPointList())
                && index < param.getMRestrictedArea().getMPointList().size() ) {
            previewParams.setPoints(param.getMRestrictedArea().getMPointList().get(index));
        }
        previewParams.setScreenLeft(1200);
        previewParams.setScreenRight(600);
        previewParams.setScreenTop(210);
        previewParams.setScreenBottom(140);
        mMapAdapter.showPreview(mapTypeId, previewParams);
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
