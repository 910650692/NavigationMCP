package com.sgm.navi.scene.ui.navi;

import static com.sgm.navi.service.MapDefaultFinalTag.MAP_TOUCH;

import android.content.Context;
import android.os.Bundle;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.RoutePath;
import com.sgm.navi.scene.api.navi.INaviViaItemClickListener;
import com.sgm.navi.scene.databinding.SceneNaviViaListViewBinding;
import com.sgm.navi.scene.impl.imersive.ImersiveStatus;
import com.sgm.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.sgm.navi.scene.impl.navi.SceneNaviViaListImpl;
import com.sgm.navi.scene.impl.navi.TimerHelper;
import com.sgm.navi.scene.impl.search.SearchFragmentFactory;
import com.sgm.navi.scene.ui.adapter.NaviViaListAdapter;
import com.sgm.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneBase;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneId;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneManager;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.navi.NaviConstant;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navi.FyChargingStation;
import com.sgm.navi.service.define.navi.FyElecVehicleETAInfo;
import com.sgm.navi.service.define.navi.NaviViaEntity;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.logicpaket.navi.OpenApiHelper;
import com.sgm.navi.service.logicpaket.signal.SignalPackage;
import com.sgm.navi.ui.base.BaseFragment;

import java.util.ArrayList;
import java.util.List;

/**
 * 终点、途径点列表
 *
 * @author sgm
 * @version $Revision.*$
 */
public class SceneNaviViaListView extends NaviSceneBase<SceneNaviViaListViewBinding, SceneNaviViaListImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_SCENE_VIA_LIST;
    private NaviViaListAdapter mNaviViaListAdapter;

    List<FyElecVehicleETAInfo> mElectVehicleETAInfoList;

    List<NaviViaEntity> mNaviViaEntityList;

    public SceneNaviViaListView(@NonNull final Context context) {
        super(context);
    }

    public SceneNaviViaListView(@NonNull final Context context,
                                @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviViaListView(@NonNull final Context context, @Nullable final AttributeSet attrs,
                                final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    public NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_SCENE_VIA_POINT_LIST;
    }

    protected void init() {
        super.init();
        setScreenId(MapType.MAIN_SCREEN_MAIN_MAP);
        mElectVehicleETAInfoList = new ArrayList<>();
        mNaviViaEntityList = new ArrayList<>();
    }

    @Override
    public void show() {
        super.show();
        if (null != mScreenViewModel) {
            mScreenViewModel.initTimer();
        }
        OpenApiHelper.enterPreview(mMapTypeId);
        Logger.d(TAG, MAP_TOUCH);
        ImmersiveStatusScene.getInstance().setImmersiveStatus(mMapTypeId, ImersiveStatus.TOUCH);
    }

    @Override
    public void hide() {
        super.hide();
        OpenApiHelper.exitPreview(mMapTypeId);
        // taskId：1015285 途经点收起后需要关闭继续导航按钮
        NaviSceneManager.getInstance().notifySceneStateChange(INaviSceneEvent.SceneStateChangeType.
                SceneCloseState, NaviSceneId.NAVI_CONTINUE);
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP,
                ImersiveStatus.IMERSIVE);
    }

    @Override
    public void close() {
        ThreadManager.getInstance().postUi(() -> {
            super.close();
            OpenApiHelper.exitPreview(mMapTypeId);
            if (mScreenViewModel != null) {
                mScreenViewModel.updateSceneVisible(false);
            }
            // taskId：1015285 途经点收起后需要关闭继续导航按钮
            NaviSceneManager.getInstance().notifySceneStateChange(INaviSceneEvent.SceneStateChangeType.
                    SceneCloseState, NaviSceneId.NAVI_CONTINUE);
            ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP,
                    ImersiveStatus.IMERSIVE);
        });
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        mISceneCallback = null;
    }

    @Override
    protected SceneNaviViaListViewBinding createViewBinding(final LayoutInflater inflater,
                                                            final ViewGroup viewGroup) {
        return SceneNaviViaListViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviViaListImpl initSceneImpl() {
        return new SceneNaviViaListImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setNaviViaList(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        Logger.d(TAG, "SceneNaviListView initObserver");
        final LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mViewBinding.srvAddVia.setLayoutManager(layoutManager);

        mNaviViaListAdapter = new NaviViaListAdapter();
        mViewBinding.srvAddVia.setAdapter(mNaviViaListAdapter);
        mNaviViaListAdapter.setOnItemClickListener(new INaviViaItemClickListener() {
            @Override
            public void onItemClick(final int position, final NaviViaEntity entity) {
                Logger.i(TAG, "onItemClick:", position, " isUserAdd:", entity.isUserAdd());
                if (mNaviViaListAdapter.getData().size() <= 1) {
                    return;
                }
                if (mISceneCallback != null) {
                    final Fragment fragment = (Fragment) ARouter.getInstance()
                            .build(RoutePath.Search.POI_DETAILS_FRAGMENT)
                            .navigation();
                    final PoiInfoEntity poiInfo = new PoiInfoEntity();
                    poiInfo.setPid(entity.getPid());
                    poiInfo.setPoint(entity.getRealPos());
                    Bundle bundle = SearchFragmentFactory.
                            createPoiDetailsFragment(
                                    AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT,
                                    AutoMapConstant.PoiType.POI_DELETE_AROUND, poiInfo);
                    bundle.putInt(NaviConstant.NAVI_CONTROL, 1);
                    if (!entity.isUserAdd()) {//过滤非补能点
                        bundle.putInt(NaviConstant.VIA_POSITION, position);
                    }
                    addFragment((BaseFragment) fragment, bundle, false);
                    mISceneCallback.hideNaviContent();
                    resetTimer();
                }
            }

            @Override
            public void onDelClick(final int position, final NaviViaEntity entity) {
                Logger.i(TAG, "onDelClick", "position:", position, "callBack is null :",
                        (mISceneCallback == null));
                if (mISceneCallback != null) {
                    Logger.i(TAG, "entity:", (entity == null ? "null" : entity.getPid() +
                            ":" + entity.getName()));
                    mISceneCallback.deleteViaPoint(entity);
                    resetTimer();
                }
            }
        });

        mViewBinding.srvAddVia.addOnScrollListener(new RecyclerView.OnScrollListener() {
            @Override
            public void onScrolled(@NonNull RecyclerView recyclerView, int dx, int dy) {
                super.onScrolled(recyclerView, dx, dy);
                resetTimer();
            }
        });

    }

    /**
     * 操作页面后重新计时
     */
    private void resetTimer() {
        Logger.i(TAG, "resetTimer");
        // 加入防暴力操作
        if (null != mScreenViewModel && TimerHelper.isCanDo()) {
            mScreenViewModel.initTimer();
            ImmersiveStatusScene.getInstance().setImmersiveStatus(
                    MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.TOUCH);
        }
    }

    /**
     * 展示途径点集合
     */
    public void showNaviViaList(final boolean isVisible) {
        Logger.i(TAG, "showNaviViaList:", isVisible);
        if (mScreenViewModel == null) {
            Logger.e(TAG, "mScreenViewModel == null");
            return;
        }
        mScreenViewModel.updateSceneVisible(isVisible);
    }

    /**
     * 更新途径点集合
     *
     * @param list 新途径点集合
     */
    public void updateViaListState(final List<NaviViaEntity> list) {
        if (ConvertUtils.isEmpty(list) || null == mScreenViewModel ||
                null == mNaviViaListAdapter) return;
        addBatteryLeftData(list);
        mNaviViaEntityList.clear();
        mNaviViaEntityList.addAll(list);
        // 如果是电车在当前页面不立即更新数据，因为有电量数据要更新，直接根据电车回调更新整体数据
        if (OpenApiHelper.powerType() == 1 && this.isVisible()) {
            return;
        }
        notifyList();
    }

    /**
     * @param result 删除途经点提醒
     * @param entity entity
     */
    public void notifyDeleteViaPointResult(final boolean result, final NaviViaEntity entity) {
        if (entity == null) {
            Logger.i(TAG, "result:", result, " entity == null");
            return;
        }
        Logger.i(TAG, "result:", result, " pid:", entity.getPid(), " name:",
                entity.getName(), " address:", entity.getAddress());
        if (result) {
            mNaviViaListAdapter.removeData(entity);
        }
    }

    public void updateElectVehicleETAInfo(final List<FyElecVehicleETAInfo> infos) {
        mElectVehicleETAInfoList.clear();
        mElectVehicleETAInfoList.addAll(infos);
        addBatteryLeftData(mNaviViaEntityList);
        notifyList();
    }

    private void notifyList() {
        if (!ConvertUtils.isEmpty(mNaviViaEntityList)) {
            if (mISceneCallback != null) {
                mNaviViaListAdapter.setIsViaArrived(mISceneCallback.getIsViaArrived());
            }
            mNaviViaListAdapter.notifyList(mNaviViaEntityList);
        }
    }

    /**
     * 添加电池剩余电量数据
     */
    private void addBatteryLeftData(List<NaviViaEntity> list) {
        if (OpenApiHelper.powerType() != 1) {
            Logger.i(TAG, "非纯电车不更新电池剩余电量数据");
            return;
        }
        if (!ConvertUtils.isEmpty(mElectVehicleETAInfoList)) {
            long currentPathId = OpenApiHelper.getCurrentPathId(mMapTypeId);
            FyElecVehicleETAInfo currentVehicleETAInfo = null;
            for (FyElecVehicleETAInfo info : mElectVehicleETAInfoList) {
                if (info.getPathID() == currentPathId) {
                    currentVehicleETAInfo = info;
                    break;
                }
            }
            if (null != currentVehicleETAInfo) {
                // 手动添加的途经点和终点的电量信息
                ArrayList<Long> batteryLeftUserAdd = new ArrayList<>(
                        currentVehicleETAInfo.getEnergySum());
                // 补能点的电量信息
                ArrayList<FyChargingStation> chargeStationInfo =
                        new ArrayList<>(currentVehicleETAInfo.getChargeStationInfo());
                //单位转换千瓦时转为百分之一瓦时
                long currentEnergy = (long) (SignalPackage.getInstance().getBatteryEnergy() *
                        100000);
                long maxEnergy = (long) (SignalPackage.getInstance().getMaxBatteryEnergy() *
                        100000);
                int indexUserAdd = 0, indexCharge = 0;
                if (!ConvertUtils.isEmpty(list)) {
                    for (NaviViaEntity naviViaEntity : list) {
                        if (naviViaEntity.isUserAdd() &&
                                !ConvertUtils.isEmpty(batteryLeftUserAdd) &&
                                indexUserAdd < batteryLeftUserAdd.size()) {
                            int chargeLeft = OpenApiHelper.
                                    calculateRemainingOrNeededEnergyPercent(
                                            batteryLeftUserAdd.get(indexUserAdd++), currentEnergy,
                                            maxEnergy);
                            naviViaEntity.setArriveBatteryLeft(chargeLeft);
                        } else if (!naviViaEntity.isUserAdd() &&
                                !ConvertUtils.isEmpty(chargeStationInfo) &&
                                indexCharge < chargeStationInfo.size()) {
                            int chargeLeft = OpenApiHelper.
                                    calculateRemainingOrNeededEnergyPercent(
                                            chargeStationInfo.get(indexCharge++).
                                                    getChargeEnrgySum(), currentEnergy, maxEnergy);
                            naviViaEntity.setArriveBatteryLeft(chargeLeft);
                        }
                    }
                }
            }
        }
    }

    public void updateSceneVisible(final boolean isVisible) {
        Logger.i(TAG, "updateSceneVisible:", isVisible);
        if (mScreenViewModel != null) {
            mScreenViewModel.updateSceneVisible(isVisible);
        }
    }
}
