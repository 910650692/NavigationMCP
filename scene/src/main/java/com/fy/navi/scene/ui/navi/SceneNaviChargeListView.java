package com.fy.navi.scene.ui.navi;

import android.content.Context;
import android.os.Looper;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.databinding.NaviSceneRecommedChargeListBinding;
import com.fy.navi.scene.impl.navi.SceneChargeListViewImpl;
import com.fy.navi.scene.ui.adapter.NaviChargeStationAdapter;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;

import java.util.ArrayList;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/2
 * Description: [在这里描述文件功能]
 */
public class SceneNaviChargeListView extends NaviSceneBase<NaviSceneRecommedChargeListBinding, SceneChargeListViewImpl> {
    private static final String TAG = "SceneChargeListView";
    private NaviChargeStationAdapter adapter;
    private SearchResultEntity mEntity;
    private final int mMaxSize = 3;
    private RecyclerView.OnScrollListener mScrollChangeListener = new RecyclerView.OnScrollListener() {
        @Override
        public void onScrollStateChanged(@NonNull RecyclerView recyclerView, int newState) {
            super.onScrollStateChanged(recyclerView, newState);
            Logger.d(TAG, "onScrollStateChanged:" + newState);
            switch (newState) {
                case RecyclerView.SCROLL_STATE_IDLE -> {
                    resetCountdown();
                    startCountdown();
                }
                default -> {
                    cancelCountdown();
                }
            }
        }
    };

    public SceneNaviChargeListView(@NonNull Context context) {
        super(context);
    }

    public SceneNaviChargeListView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviChargeListView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    public NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_PROVIDE_CHARGE_LIST;
    }

    @Override
    protected void init() {
        super.init();
        mViewBinding.recyclerView.addOnScrollListener(mScrollChangeListener);
        adapter = new NaviChargeStationAdapter(getContext(), new ArrayList<>());
        mViewBinding.recyclerView.setLayoutManager(new LinearLayoutManager(getContext()));
        mViewBinding.recyclerView.setAdapter(adapter);
        adapter.setOnItemClickListener(new NaviChargeStationAdapter.OnItemClickListener() {
            @Override
            public void navi(int index) {
                if (!ConvertUtils.isNull(mISceneCallback) && !ConvertUtils.isNull(mEntity)) {
                    if (ConvertUtils.isEmpty(mEntity.getPoiList()) || index >= mEntity.getPoiList().size()) {
                        return;
                    }
                    mISceneCallback.startNaviRightNow(mEntity.getPoiList().get(index));
                    notifySceneStateChange(false);
                }
            }

            @Override
            public void onItemSelect(int index) {
                mScreenViewModel.showPreview(adapter.getData());
            }
        });
        mViewBinding.ivClose.setOnClickListener(v -> {
            notifySceneStateChange(false);
        });
    }

    @Override
    protected NaviSceneRecommedChargeListBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return NaviSceneRecommedChargeListBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneChargeListViewImpl initSceneImpl() {
        return new SceneChargeListViewImpl(this);
    }

    @Override
    protected void setInitVariableId() {

    }

    @Override
    protected void initObserver() {

    }

    /***
     *
     * @param entity
     */
    public void updateUi(final SearchResultEntity entity, final MapType mapType) {
        Logger.i(TAG, "updateUi-isMainThread:" + (Looper.myLooper() == Looper.getMainLooper()));
        ThreadManager.getInstance().postUi(() -> {
            mEntity = entity;
            setScreenId(mapType);
            if (ConvertUtils.isNull(entity) || ConvertUtils.isEmpty(entity.getPoiList()) || adapter == null) {
                Logger.i(TAG, "updateUi failed:", "entity is null or poiList is empty", "adapter:" + ConvertUtils.isNull(adapter));
                return;
            }
            Logger.i(TAG, "size:" + entity.getPoiList().size());
            // 最多显示3个
            adapter.updateData(entity.getPoiList().size() > mMaxSize ? entity.getPoiList().subList(0, mMaxSize) : entity.getPoiList());
            notifySceneStateChange(true);
        });
    }

    @Override
    public void show() {
        super.show();
        mScreenViewModel.showPreview(adapter.getData());
    }

    @Override
    public void close() {
        super.close();
        mScreenViewModel.naviContinue();
    }

    @Override
    public void hide() {
        super.hide();
        mScreenViewModel.naviContinue();
    }

    @Override
    public void onDestroy() {
        if (!ConvertUtils.isNull(mScrollChangeListener)) {
            mViewBinding.recyclerView.removeOnScrollListener(mScrollChangeListener);
            mScrollChangeListener = null;
        }
        super.onDestroy();
    }

    public int getSelectIndex() {
        return adapter.getSelectIndex();
    }

    public void updateSelect(final PoiInfoEntity poiInfoEntity) {
        adapter.updateSelect(poiInfoEntity);
    }

    @Override
    public boolean isNeedAutoStartTimer() {
        return true;
    }
}
