package com.fy.navi.scene.ui.navi;

import android.content.Context;
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
import com.fy.navi.scene.databinding.NaviSceneRecommedGasListBinding;
import com.fy.navi.scene.impl.navi.SceneGasListViewImpl;
import com.fy.navi.scene.ui.adapter.NaviGasStationAdapter;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;

import java.util.ArrayList;
import java.util.List;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/2
 * Description: [推荐加油站列表]
 */
public class SceneGasListView extends NaviSceneBase<NaviSceneRecommedGasListBinding, SceneGasListViewImpl> {
    private static final String TAG = "SceneGasListView";
    private NaviGasStationAdapter adapter;
    private SearchResultEntity mEntity;
    private static final int MAX_SIZE = 3;
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

    public SceneGasListView(@NonNull Context context) {
        super(context);
    }

    public SceneGasListView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneGasListView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    public NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_PROVIDE_GAS_LIST;
    }

    @Override
    protected void init() {
        super.init();
        mViewBinding.recyclerView.addOnScrollListener(mScrollChangeListener);
        adapter = new NaviGasStationAdapter(getContext(), new ArrayList<>());
        mViewBinding.recyclerView.setLayoutManager(new LinearLayoutManager(getContext()));
        mViewBinding.recyclerView.setAdapter(adapter);
        adapter.setOnItemClickListener(new NaviGasStationAdapter.OnItemClickListener() {
            @Override
            public void navi(int index) {
                if (!ConvertUtils.isNull(mISceneCallback) && !ConvertUtils.isNull(mEntity)) {
                    if (ConvertUtils.isEmpty(mEntity.getPoiList()) || index >= mEntity.getPoiList().size()) {
                        return;
                    }
                    mISceneCallback.startNaviRightNow(mEntity.getPoiList().get(index));
                    getNaviSceneEvent().notifySceneStateChange(INaviSceneEvent.SceneStateChangeType.SceneCloseState, getSceneId());
                }
            }

            @Override
            public void onItemSelect(int index) {
                mScreenViewModel.showPreview(adapter.getData());
            }
        });
    }


    @Override
    protected NaviSceneRecommedGasListBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return NaviSceneRecommedGasListBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneGasListViewImpl initSceneImpl() {
        return new SceneGasListViewImpl(this);
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
        ThreadManager.getInstance().postUi(() -> {
            setScreenId(mapType);
            Logger.i(TAG, "updateUi:");
            if (ConvertUtils.isNull(entity) || ConvertUtils.isEmpty(entity.getPoiList()) || adapter == null) {
                Logger.i(TAG, "updateUi failed:", "entity is null or poiList is empty", "adapter:" + ConvertUtils.isNull(adapter));
                return;
            }
            Logger.i(TAG, "size:" + entity.getPoiList().size());
            final List<PoiInfoEntity> tmList = entity.getPoiList().size() > MAX_SIZE ? entity.getPoiList().subList(0, MAX_SIZE) : entity.getPoiList();
            adapter.updateData(tmList);
            mEntity = entity;
            notifySceneStateChange(true);
        });
    }

    @Override
    public void show() {
        super.show();
        mScreenViewModel.showPreview(adapter.getData());
    }

    @Override
    public void onDestroy() {
        if (!ConvertUtils.isNull(mScrollChangeListener)) {
            mViewBinding.recyclerView.removeOnScrollListener(mScrollChangeListener);
            mScrollChangeListener = null;
        }
        super.onDestroy();
    }

    @Override
    public boolean isNeedAutoStartTimer() {
        return true;
    }

    public int getSelectIndex() {
        return adapter.getSelectIndex();
    }
}
