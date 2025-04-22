package com.fy.navi.scene.ui.navi.hangingcard;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.fy.navi.scene.databinding.NaviSceneHandingCardDetailBinding;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.scene.impl.navi.NaviSceneHandingCardDetailImpl;
import com.fy.navi.scene.ui.adapter.HandingCardDetailAdapter;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.scene.util.HandCardType;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.List;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/19
 * Description: [悬挂卡详情]
 */
public class NaviSceneHandingCardDetail extends NaviSceneBase<NaviSceneHandingCardDetailBinding, NaviSceneHandingCardDetailImpl> {
    private HandingCardDetailAdapter mAdapter;

    public NaviSceneHandingCardDetail(@NonNull Context context) {
        super(context);
    }

    public NaviSceneHandingCardDetail(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public NaviSceneHandingCardDetail(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_SUSPEND_CARD_DETAIL;
    }

    @Override
    protected NaviSceneHandingCardDetailBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return NaviSceneHandingCardDetailBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected NaviSceneHandingCardDetailImpl initSceneImpl() {
        return new NaviSceneHandingCardDetailImpl(this);
    }

    @Override
    protected void setInitVariableId() {

    }

    @Override
    protected void initObserver() {
        mAdapter = new HandingCardDetailAdapter(getContext(), mScreenViewModel);
        mViewBinding.recyclerView.setLayoutManager(new LinearLayoutManager(getContext()));
        mViewBinding.recyclerView.setAdapter(mAdapter);
        mViewBinding.ivClose.setOnClickListener(v -> {
            mScreenViewModel.exitPreview();
            notifySceneStateChange(false);
        });
        mViewBinding.recyclerView.addOnScrollListener(new RecyclerView.OnScrollListener() {
            @Override
            public void onScrollStateChanged(@NonNull RecyclerView recyclerView, int newState) {
                super.onScrollStateChanged(recyclerView, newState);
                switch (newState) {
                    case RecyclerView.SCROLL_STATE_IDLE -> {
                        resetCountdown();
                        startCountdown();
                    }
                    default -> {
                        cancelCountdown();
                        ImmersiveStatusScene.getInstance().setImmersiveStatus(
                                mMapTypeId, ImersiveStatus.TOUCH);
                        NaviSceneManager.getInstance().notifySceneStateChange(
                                INaviSceneEvent.SceneStateChangeType.SceneHideState,
                                NaviSceneId.NAVI_SCENE_CONTROL
                        );
                    }
                }
            }
        });
    }

    public void updateUi(List<PoiInfoEntity> list, HandCardType type) {
        mScreenViewModel.updateUi(list, type);
        mAdapter.notifyDataChanged(list, type);
        notifySceneStateChange(true);
    }

    @Override
    public void show() {
        super.show();
        mScreenViewModel.showPreview(mAdapter.getSelectIndex());
    }

    @Override
    public void hide() {
        super.hide();
        mScreenViewModel.exitPreview();
    }

    @Override
    public void close() {
        mScreenViewModel.exitPreview();
        super.close();
    }

    @Override
    public boolean isNeedAutoStartTimer() {
        return true;
    }
}
