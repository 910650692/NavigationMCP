package com.sgm.navi.scene.ui.search;

import android.animation.ValueAnimator;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.LinearInterpolator;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.android.utils.ConvertUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.BaseSceneView;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.TerminalParkingResultViewBinding;
import com.sgm.navi.scene.impl.search.SceneTerminalViewImpl;
import com.sgm.navi.scene.ui.adapter.TerminalParkingResultAdapter;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.SearchResultEntity;

import java.util.List;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: SceneTerminalParkingListView
 * @CreateDate: 2025/3/5 16:32
 */
public class SceneTerminalParkingListView extends BaseSceneView<TerminalParkingResultViewBinding, SceneTerminalViewImpl> {
    private TerminalParkingResultAdapter mAdapter;
    private LinearLayoutManager layoutManager;
    private int mIndex;
    private PoiInfoEntity mPoiInfoEntity;
    private ValueAnimator mAnimator;
    private float mAngelTemp = 0;
    private GeoPoint mPoint;

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
        initLoadAnim(mViewBinding.ivLoading);

    }

    /**
     * 初始化加载动画
     * @param sivLoading 加载动画视图
     */
    private void initLoadAnim(final View sivLoading) {
        // 如果动画已存在并正在运行，则取消并清理
        if (mAnimator != null) {
            if (mAnimator.isRunning()) {
                mAnimator.cancel();
            }
            mAnimator = null;
        }

        // 创建属性动画，从 0 到 360 度循环旋转
        mAnimator = ValueAnimator.ofFloat(0f, 360f);
        mAnimator.setDuration(2000); // 动画持续时间
        mAnimator.setRepeatCount(ValueAnimator.INFINITE); // 无限重复
        mAnimator.setInterpolator(new LinearInterpolator()); // 线性插值器
        // 添加动画更新监听器
        mAnimator.addUpdateListener(animation -> {
            final float angle = (float) animation.getAnimatedValue();
            if (shouldSkipUpdate(angle)) {
                return;
            }
            sivLoading.setRotation(angle);
        });
    }

    /**
     *用于控制角度变化频率的辅助方法
     *@param angle 当前角度
     *@return 是否跳过更新
     */
    private boolean shouldSkipUpdate(final float angle) {
        final float changeAngle = angle - mAngelTemp;
        final float angleStep = 10;
        if (changeAngle > 0f && changeAngle <= angleStep) {
            return true; // 跳过更新，避免高频调用浪费资源
        }
        mAngelTemp = angle; // 更新临时角度值
        return false;
    }

    public void showLoading(final boolean isShow){
        if(ConvertUtils.isNull(mViewBinding)){
            return;
        }
        mViewBinding.ivLoading.setVisibility(isShow ? View.VISIBLE : View.GONE);
        mViewBinding.noResultHint.setVisibility(isShow ? View.VISIBLE : View.GONE);
        mViewBinding.noResultHint.setText(getContext().getString(R.string.st_search_loading));
        mViewBinding.noResultButton.setVisibility(View.GONE);
        mViewBinding.recyclerSearchResult.setVisibility(isShow ? GONE : View.VISIBLE);
        if (!ConvertUtils.isNull(mAnimator)) {
            if (isShow && !mAnimator.isRunning()) {
                mAnimator.start();
            } else {
                mAnimator.cancel();
            }
        }
    }

    private final Runnable mTimeoutTask = new Runnable() {
        @Override
        public void run() {
            if (!ConvertUtils.isEmpty(mViewBinding)) {
                mViewBinding.noResultButton.setVisibility(View.VISIBLE);
                mViewBinding.noResultHint.setText(getContext().getString(R.string.load_failed));
                mViewBinding.ivLoading.setVisibility(View.GONE);
                mViewBinding.recyclerSearchResult.setVisibility(View.GONE);
                if (mAnimator != null) {
                    mAnimator.cancel();
                }
                mViewBinding.noResultButton.setOnClickListener((view) -> {
                    if (mPoint != null) {
                        aroundSearch(mPoint);
                    }
                });
            }
        }
    };

    /**
     * 周边搜索
     * @param geoPoint 目标POI坐标点 经纬度
     */
    public void aroundSearch(final GeoPoint geoPoint) {
        mPoint = geoPoint;
        mScreenViewModel.aroundSearch("停车场", geoPoint);
        if(!ConvertUtils.isNull(mAdapter)){
            mAdapter.setEndPoint(geoPoint);
        }
        showLoading(true);
        ThreadManager.getInstance().removeHandleTask(mTimeoutTask);
        ThreadManager.getInstance().postDelay(mTimeoutTask, 6000);
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
                mIndex = position;
                mPoiInfoEntity = poiInfoEntity;
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
            mScreenViewModel.setSelectIndex(mPoiInfoEntity, mIndex, false);
        });
    }

    /**
     * 更新搜索结果
     * @param taskId 任务id
     * @param searchResultEntity 搜索结果实体类
     */
    public void notifySearchResult(final int taskId, final SearchResultEntity searchResultEntity) {
        ThreadManager.getInstance().removeHandleTask(mTimeoutTask);
        if (searchResultEntity == null || searchResultEntity.getPoiList().isEmpty()) {
            ToastUtils.Companion.getInstance().showCustomToastView("暂无数据");
            ThreadManager.getInstance().postDelay(mTimeoutTask, 0);
            return;
        }
        showLoading(false);
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
                        mPoiInfoEntity = poiInfo;
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
                mAdapter.updateSelectedPosition(index);
                layoutManager.scrollToPositionWithOffset(index, 0);
                PoiInfoEntity finalMinPoi = minPoi;
                int finalIndex = index;
                // 延迟1s在去选中下标，等待图层渲染完毕
                ThreadManager.getInstance().postDelay(() -> {
                    if (mScreenViewModel != null) {
                        mPoiInfoEntity = finalMinPoi;
                        mIndex = finalIndex;
                        mScreenViewModel.setSelectIndex(finalMinPoi, finalIndex);
                    }
                },1000);
            }
        });
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        ThreadManager.getInstance().removeHandleTask(mTimeoutTask);
        if (!ConvertUtils.isNull(mAnimator)) {
            mAnimator.cancel();
        }
    }

    public void onBackPressed() {
        if (null != mScreenViewModel) mScreenViewModel.closeSearch();
    }
}
