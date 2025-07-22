package com.sgm.navi.scene.ui.search;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.android.utils.ConvertUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.BaseSceneView;
import com.sgm.navi.scene.databinding.TerminalParkingResultViewBinding;
import com.sgm.navi.scene.impl.search.SceneTerminalViewImpl;
import com.sgm.navi.scene.ui.adapter.TerminalParkingResultAdapter;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.logicpaket.search.SearchPackage;

import java.util.List;
import java.util.Optional;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: SceneTerminalParkingListView
 * @CreateDate: 2025/3/5 16:32
 */
public class SceneTerminalParkingListView extends BaseSceneView<TerminalParkingResultViewBinding, SceneTerminalViewImpl> {
    private TerminalParkingResultAdapter mAdapter;
    private LinearLayoutManager layoutManager;
    private SearchLoadingDialog mSearchLoadingDialog;
    private int mIndex;

    public void setIndex(final int index) {
        mIndex = index;
    }

    public SceneTerminalParkingListView(@NonNull final Context context) {
        super(context);
    }

    public SceneTerminalParkingListView(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneTerminalParkingListView(@NonNull final Context context, @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected TerminalParkingResultViewBinding createViewBinding(final LayoutInflater inflater, final ViewGroup viewGroup) {
        return TerminalParkingResultViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneTerminalViewImpl initSceneImpl() {
        return new SceneTerminalViewImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setSceneTerminalViewImpl(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        setupRecyclerView();
        setupSearchActions();
        mSearchLoadingDialog = new SearchLoadingDialog(getContext());

    }

    /**
     * 周边搜索
     * @param geoPoint 目标POI坐标点 经纬度
     */
    public void aroundSearch(final GeoPoint geoPoint) {
        mScreenViewModel.aroundSearch("停车场", geoPoint);
        if(!ConvertUtils.isNull(mAdapter)){
            mAdapter.setEndPoint(geoPoint);
        }
        if (null != mSearchLoadingDialog && mSearchLoadingDialog.isShowing()) {
            Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG, "mSearchLoadingDialog is showing");
        } else {
            mSearchLoadingDialog = new SearchLoadingDialog(getContext());
            mSearchLoadingDialog.show();
        }
    }

    /**
     * 终点停车场扎标点击事件
     * @param index 点击下标
     */
    public void onMarkTerminalParkClickCallBack(final int index) {
        if (mAdapter != null) {
            mAdapter.updateSelectedPosition(index);
            layoutManager.scrollToPositionWithOffset(index, 0);
        }
    }

    /**
     * 初始化 RecyclerView
     */
    private void setupRecyclerView() {
        layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mViewBinding.recyclerSearchResult.setLayoutManager(layoutManager);

        mAdapter = new TerminalParkingResultAdapter();
        mViewBinding.recyclerSearchResult.setAdapter(mAdapter);
        mAdapter.setOnItemClickListener(new TerminalParkingResultAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(final int position, final PoiInfoEntity poiInfoEntity) {
                mScreenViewModel.setSelectIndex(poiInfoEntity, position);
            }

            @Override
            public void onNaviClick(final int position, final PoiInfoEntity poiInfoEntity) {
                mScreenViewModel.startRoute(poiInfoEntity);
            }
        });
    }

    /**
     * 搜索相关事件
     */
    private void setupSearchActions() {
        mViewBinding.skIvTerminalClose.setOnClickListener(v -> {
            mScreenViewModel.showRoutePark();
            mScreenViewModel.closeSearch();
        });
    }

    /**
     * 更新搜索结果
     * @param taskId 任务id
     * @param searchResultEntity 搜索结果实体类
     */
    public void notifySearchResult(final int taskId, final SearchResultEntity searchResultEntity) {
        if (searchResultEntity == null || searchResultEntity.getPoiList().isEmpty()) {
            ToastUtils.Companion.getInstance().showCustomToastView("暂无数据");
            return;
        }
        if (mSearchLoadingDialog != null && mSearchLoadingDialog.isShowing()) {
            mSearchLoadingDialog.dismiss();
        }
        if (ConvertUtils.isEmpty(mScreenViewModel)) {
            return;
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "taskId: " , taskId
                , " currentId: " , mScreenViewModel.getMTaskId());
        if (!ConvertUtils.equals(taskId, mScreenViewModel.getMTaskId()) && mScreenViewModel.getMTaskId() != 0) {
            return;
        }
        if (mAdapter != null) {
            mAdapter.notifyList(searchResultEntity);
            if (mIndex != -1 && mIndex < searchResultEntity.getPoiList().size()) {
                mAdapter.updateSelectedPosition(mIndex);
                layoutManager.scrollToPositionWithOffset(mIndex, 0);
                PoiInfoEntity poiInfo = searchResultEntity.getPoiList().get(mIndex);
                ThreadManager.getInstance().postDelay(() -> {
                    if (mScreenViewModel != null) {
                        mScreenViewModel.setSelectIndex(poiInfo, mIndex);
                    }
                },1000);
            } else {
                setMinDistancePark(searchResultEntity.getPoiList());
            }
        }
    }

    private void setMinDistancePark(List<PoiInfoEntity> poiList) {
        mViewBinding.recyclerSearchResult.post(new Runnable() {
            @Override
            public void run() {
                PoiInfoEntity minPoi = poiList.get(0);
                int index = 0;
//                for (int i = 1; i < poiList.size(); i++) {
//                    int curParkDistance = SearchPackage.getInstance().calcStraightDistanceWithInt(poiList.get(i).getMPoint());
//                    int minParkDistance = SearchPackage.getInstance().calcStraightDistanceWithInt(minPoi.getMPoint());
//                    if(curParkDistance < minParkDistance){
//                        minPoi = poiList.get(i);
//                        index = i;
//                    }
//                }
                mAdapter.updateSelectedPosition(index);
                layoutManager.scrollToPositionWithOffset(index, 0);
                PoiInfoEntity finalMinPoi = minPoi;
                int finalIndex = index;
                // 延迟1s在去选中下标，等待图层渲染完毕
                ThreadManager.getInstance().postDelay(() -> {
                    if (mScreenViewModel != null) {
                        mScreenViewModel.setSelectIndex(finalMinPoi, finalIndex);
                    }
                },1000);
            }
        });
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (mSearchLoadingDialog != null && mSearchLoadingDialog.isShowing()) {
            mSearchLoadingDialog.dismiss();
        }
    }
}
