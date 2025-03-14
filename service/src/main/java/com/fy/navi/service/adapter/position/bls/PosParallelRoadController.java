package com.fy.navi.service.adapter.position.bls;

import androidx.annotation.NonNull;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.pos.PosService;
import com.autonavi.gbl.pos.model.LocParallelRoad;
import com.autonavi.gbl.pos.model.LocParallelRoadInfo;
import com.autonavi.gbl.pos.observer.IPosParallelRoadObserver;
import com.autonavi.gbl.pos.observer.IPosSwitchParallelRoadObserver;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.position.IPositionAdapterCallback;
import com.fy.navi.service.define.position.LocParallelInfoEntity;
import com.fy.navi.service.define.position.LocalParallelRoadEntity;

import java.util.ArrayList;
import java.util.List;

/*平行路切换工具类*/
public class PosParallelRoadController implements IPosSwitchParallelRoadObserver, IPosParallelRoadObserver {
    private static final String TAG = MapDefaultFinalTag.POSITION_SERVICE_TAG;
    private PosService mPosService;
    private List<IPositionAdapterCallback> mList;

    public PosParallelRoadController(PosService posService, List<IPositionAdapterCallback> list) {
        this.mPosService = posService;
        this.mList = list;
    }

    public void addObserver() {
        // 添加切换平行路观察者
        mPosService.addSwitchParallelRoadObserver(this);
        // 添加平行路信息观察者
        mPosService.addParallelRoadObserver(this);
    }

    public void removeObserver() {
        // 添加切换平行路观察者
        mPosService.removeSwitchParallelRoadObserver(this);
        // 添加平行路信息观察者
        mPosService.removeParallelRoadObserver(this);
    }

    @Override
    public void onSwitchParallelRoadFinished() {
        if (!ConvertUtils.isEmpty(mList)) {
            Logger.i(TAG, "mList：" + mList.size());
            for (IPositionAdapterCallback callback : mList) {
                callback.onSwitchParallelRoadFinished();
            }
        } else {
            Logger.e(TAG, "mList is null");
        }
    }

    @Override
    public void onParallelRoadUpdate(LocParallelRoadInfo locParallelRoadInfo) {
        if (!ConvertUtils.isEmpty(mList)) {
            Logger.i(TAG, "mList：" + mList.size());
            for (IPositionAdapterCallback callback : mList) {
                callback.onParallelRoadUpdate(getLocParallelInfoEntity(locParallelRoadInfo));
            }
        } else {
            Logger.e(TAG, "mList is null");
        }
    }

    /**
     * 数据转换
     * @param locParallelRoadInfo 平行路信息
     * @return LocParallelInfoEntity
     */
    private LocParallelInfoEntity getLocParallelInfoEntity(LocParallelRoadInfo locParallelRoadInfo) {
        LocParallelInfoEntity locParallelInfoEntity = new LocParallelInfoEntity();
        if (!ConvertUtils.isEmpty(locParallelRoadInfo)) {
            locParallelInfoEntity.setFlag(locParallelRoadInfo.flag);
            locParallelInfoEntity.setStatus(locParallelRoadInfo.status);
            locParallelInfoEntity.setHwFlag(locParallelRoadInfo.hwFlag);
            if (!ConvertUtils.isEmpty(locParallelRoadInfo.parallelRoadList)) {
                ArrayList<LocalParallelRoadEntity> list = getLocalParallelRoadEntities(
                        locParallelRoadInfo);
                locParallelInfoEntity.setLocalParallelRoadArrayList(list);
            }
        }
        return locParallelInfoEntity;
    }

    @NonNull
    private static ArrayList<LocalParallelRoadEntity> getLocalParallelRoadEntities(
            LocParallelRoadInfo locParallelRoadInfo) {
        ArrayList<LocalParallelRoadEntity> list = new ArrayList<>();
        for (LocParallelRoad locParallelRoad : locParallelRoadInfo.parallelRoadList) {
            LocalParallelRoadEntity localParallelRoadEntity = new LocalParallelRoadEntity();
            localParallelRoadEntity.setRoadID(locParallelRoad.roadId);
            localParallelRoadEntity.setType(locParallelRoad.type);
            localParallelRoadEntity.setFormWay(locParallelRoad.formway);
            localParallelRoadEntity.setLinkType(locParallelRoad.linkType);
            list.add(localParallelRoadEntity);
        }
        return list;
    }
}
