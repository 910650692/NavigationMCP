package com.fy.navi.ui.view;

import android.animation.ValueAnimator;
import android.content.Context;
import android.util.AttributeSet;
import android.view.Gravity;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewConfiguration;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.core.view.ViewCompat;

import com.android.utils.ScreenUtils;
import com.android.utils.log.Logger;
import com.fy.navi.ui.view.refresh.FooterView;
import com.fy.navi.ui.view.refresh.HeadRefreshView;
import com.fy.navi.ui.view.refresh.HeadView;
import com.fy.navi.ui.view.refresh.LoadMoreView;
import com.fy.navi.ui.view.refresh.RefreshListener;
import com.fy.navi.ui.view.refresh.State;


/**
 * 上拉加载下拉刷新
 */
public class PullToRefreshLayout extends SkinFrameLayout {

    private static final long ANIM_TIME = 250;
    private static int HEAD_HEIGHT = 60;
    private static int FOOT_HEIGHT = 60;
    private static int head_height;
    private static int head_height_2;
    private static int foot_height;
    private static int foot_height_2;
    private HeadView mHeaderView;
    private FooterView mFooterView;
    private View mChildView;
    private float mTouchY;
    private float mCurrentY;

    private boolean canLoadMore = true;
    private boolean canRefresh = true;
    private boolean isRefresh;
    private boolean isLoadMore;

    //滑动的最小距离
    private int mTouchSlope;

    private RefreshListener refreshListener;

    public PullToRefreshLayout(Context context) {
        this(context, null);
    }

    public PullToRefreshLayout(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public PullToRefreshLayout(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init();
    }

    public void setRefreshListener(RefreshListener refreshListener) {
        this.refreshListener = refreshListener;
    }

    private void cal() {
        head_height = ScreenUtils.Companion.getInstance().dp2px(HEAD_HEIGHT);
        foot_height = ScreenUtils.Companion.getInstance().dp2px(FOOT_HEIGHT);
        head_height_2 = ScreenUtils.Companion.getInstance().dp2px(HEAD_HEIGHT * 2);
        foot_height_2 = ScreenUtils.Companion.getInstance().dp2px(FOOT_HEIGHT * 2);
        mTouchSlope = ViewConfiguration.get(getContext()).getScaledTouchSlop();
    }

    private void init() {
        cal();
        int count = getChildCount();
        Logger.d("SEARCH_HMI_TAG", "count:" + count);
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        mChildView = getChildAt(0);
        addHeadView();
        addFooterView();
    }

    private void addHeadView() {
        if (mHeaderView == null) {
            mHeaderView = new HeadRefreshView(getContext());
        }
        LayoutParams layoutParams = new LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, 0);
        mHeaderView.getView().setLayoutParams(layoutParams);
        if (mHeaderView.getView().getParent() != null) {
            ((ViewGroup) mHeaderView.getView().getParent()).removeAllViews();
        }
        addView(mHeaderView.getView(), 0);
    }

    private void addFooterView() {
        if (mFooterView == null) {
            mFooterView = new LoadMoreView(getContext());
        }
        LayoutParams layoutParams = new LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, 0);
        layoutParams.gravity = Gravity.BOTTOM;
        mFooterView.getView().setLayoutParams(layoutParams);
        if (mFooterView.getView().getParent() != null) {
            ((ViewGroup) mFooterView.getView().getParent()).removeAllViews();
        }
        addView(mFooterView.getView());
    }

    @Override
    public boolean onInterceptTouchEvent(MotionEvent ev) {
        if (!canLoadMore && !canRefresh) {
            return super.onInterceptTouchEvent(ev);
        }
        if (isRefresh || isLoadMore) {
            return true;
        }
        switch (ev.getAction()) {
            case MotionEvent.ACTION_DOWN:
                mTouchY = ev.getY();
                mCurrentY = mTouchY;
                break;
            case MotionEvent.ACTION_MOVE:
                float currentY = ev.getY();
                float dy = currentY - mCurrentY;
                if (canRefresh) {
                    boolean canChildScrollUp = canChildScrollUp();
                    if (dy > mTouchSlope && !canChildScrollUp) {
                        mHeaderView.begin();
                        return true;
                    }
                }
                if (canLoadMore) {
                    boolean canChildScrollDown = canChildScrollDown();
                    if (dy < -mTouchSlope && !canChildScrollDown) {
                        mFooterView.begin();
                        return true;
                    }
                }
            default:
                break;
        }
        return super.onInterceptTouchEvent(ev);
    }

    @Override
    public boolean onTouchEvent(MotionEvent event) {
        if (isRefresh || isLoadMore) {
            return true;
        }
        switch (event.getAction()) {
            case MotionEvent.ACTION_MOVE:
                mCurrentY = event.getY();
                float dura = (mCurrentY - mTouchY) / 3.0f;
                if (dura > 0 && canRefresh) {
                    dura = Math.min(head_height_2, dura);
                    dura = Math.max(0, dura);
                    mHeaderView.getView().getLayoutParams().height = (int) dura;
                    ViewCompat.setTranslationY(mChildView, dura);
                    requestLayout();
                    mHeaderView.progress(dura, head_height);
                } else {
                    dura = Math.min(foot_height_2, Math.abs(dura));
                    dura = Math.max(0, Math.abs(dura));
                    mFooterView.getView().getLayoutParams().height = (int) dura;
                    ViewCompat.setTranslationY(mChildView, -dura);
                    requestLayout();
                    if (canLoadMore) {
                        mFooterView.progress(dura, foot_height);
                    }
                }
                return true;
            case MotionEvent.ACTION_UP:
            case MotionEvent.ACTION_CANCEL:
                float currentY = event.getY();
                final int dy1 = (int) (currentY - mTouchY) / 3;
                if (dy1 > 0 && canRefresh) {
                    if (dy1 >= head_height) {
                        createAnimatorTranslationY(State.REFRESH_STATE.REFRESH,
                                dy1 > head_height_2 ? head_height_2 : dy1, head_height,
                                new CallBack() {
                                    @Override
                                    public void onSuccess() {
                                        isRefresh = true;
                                        if (refreshListener != null) {
                                            refreshListener.refresh();
                                        }
                                        mHeaderView.loading();
                                    }
                                });
                    } else if (dy1 > 0 && dy1 < head_height) {
                        setFinish(dy1, State.REFRESH_STATE.REFRESH);
                        mHeaderView.normal();
                    }
                } else {
                    if (Math.abs(dy1) >= foot_height) {
                        createAnimatorTranslationY(State.REFRESH_STATE.LOADMORE, Math.abs(dy1) > foot_height_2 ? foot_height_2 : Math.abs(dy1), foot_height,
                                new CallBack() {
                                    @Override
                                    public void onSuccess() {
                                        if (canLoadMore) {
                                            isLoadMore = true;
                                            if (refreshListener != null) {
                                                refreshListener.loadMore();
                                            }
                                            mFooterView.loading();
                                        } else {
                                            setFinish(Math.abs(dy1), State.REFRESH_STATE.LOADMORE);
                                            mFooterView.normal();
                                        }
                                    }
                                });
                    } else {
                        setFinish(Math.abs(dy1), State.REFRESH_STATE.LOADMORE);
                        mFooterView.normal();
                    }
                }
                break;
            default:
                break;
        }
        return super.onTouchEvent(event);
    }

    private boolean canChildScrollDown() {
        if (mChildView == null) {
            return false;
        }

        return ViewCompat.canScrollVertically(mChildView, 1);
    }

    private boolean canChildScrollUp() {
        if (mChildView == null) {
            return false;
        }
        return ViewCompat.canScrollVertically(mChildView, -1);
    }

    /**
     * 创建动画
     */
    public void createAnimatorTranslationY(@State.REFRESH_STATE final int state, final int start, final int purpose, final CallBack callBack) {
        final ValueAnimator anim;
        anim = ValueAnimator.ofInt(start, purpose);
        anim.setDuration(ANIM_TIME);
        anim.addUpdateListener(valueAnimator -> {
            if (valueAnimator.getAnimatedValue() != null) {
                int value = (int) valueAnimator.getAnimatedValue();
                if (state == State.REFRESH_STATE.REFRESH) {
                    mHeaderView.getView().getLayoutParams().height = value;
                    ViewCompat.setTranslationY(mChildView, value);
                    if (purpose == 0) { //代表结束加载
                        mHeaderView.finishing(value, head_height_2);
                    } else {
                        mHeaderView.progress(value, head_height);
                    }
                } else {
                    mFooterView.getView().getLayoutParams().height = value;
                    ViewCompat.setTranslationY(mChildView, -value);
                    if (purpose == 0) { //代表结束加载
                        mFooterView.finishing(value, head_height_2);
                    } else {
                        mFooterView.progress(value, foot_height);
                    }
                }
                if (value == purpose) {
                    if (callBack != null) {
                        callBack.onSuccess();
                    }
                }
                requestLayout();
            }
        });
        anim.start();
    }


    /**
     * 结束下拉刷新
     */
    private void setFinish(int height, @State.REFRESH_STATE final int state) {
        createAnimatorTranslationY(state, height, 0, () -> {
            if (state == State.REFRESH_STATE.REFRESH) {
                isRefresh = false;
                mHeaderView.normal();

            } else {
                isLoadMore = false;
                mFooterView.normal();
            }
        });
    }

    private void setFinish(@State.REFRESH_STATE int state) {
        if (state == State.REFRESH_STATE.REFRESH) {
            if (mHeaderView != null && mHeaderView.getView().getLayoutParams().height > 0 && isRefresh) {
                setFinish(head_height, state);
            }
        } else {
            if (mFooterView != null && mFooterView.getView().getLayoutParams().height > 0 && isLoadMore) {
                setFinish(foot_height, state);
            }
        }
    }

    public void setRefreshTips(String tips) {
        if (mHeaderView != null) {
            mHeaderView.setRefreshTips(tips);
        }
    }

    public void setLoadMoreTips(String tips) {
        if (mFooterView != null) {
            mFooterView.setLoadMoreTips(tips);
        }
    }

    /**
     * 结束刷新
     */
    public void finishRefresh() {
        setFinish(State.REFRESH_STATE.REFRESH);
    }

    /**
     * 结束加载更多
     */
    public void finishLoadMore() {
        setFinish(State.REFRESH_STATE.LOADMORE);
    }

    /**
     * 设置是否启用加载更多
     */
    public void setCanLoadMore(boolean canLoadMore) {
        this.canLoadMore = canLoadMore;
    }

    /**
     * 设置是否启用刷新
     */
    public void setCanRefresh(boolean canRefresh) {
        this.canRefresh = canRefresh;
    }

    public void setRefresh(boolean isRefresh) {
        if (mHeaderView != null) {
            mHeaderView.setRefresh(isRefresh);
        }
    }

    /**
     * 设置是下拉刷新头部
     *
     * @param mHeaderView 需实现 HeadView 接口
     */
    public void setHeaderView(HeadView mHeaderView) {
        this.mHeaderView = mHeaderView;
    }

    /**
     * 设置是下拉刷新尾部
     *
     * @param mFooterView 需实现 FooterView 接口
     */
    public void setFooterView(FooterView mFooterView) {
        this.mFooterView = mFooterView;
    }

    /**
     * 设置刷新控件的高度
     *
     * @param dp 单位为dp
     */
    public void setHeadHeight(int dp) {
        head_height = ScreenUtils.Companion.getInstance().dp2px(dp);
    }

    /**
     * 设置加载更多控件的高度
     *
     * @param dp 单位为dp
     */
    public void setFootHeight(int dp) {
        foot_height = ScreenUtils.Companion.getInstance().dp2px(dp);
    }

    /**
     * 同时设置加载更多控件和刷新控件的高度
     *
     * @param dp 单位为dp
     */
    public void setAllHeight(int dp) {
        head_height = ScreenUtils.Companion.getInstance().dp2px(dp);
        foot_height = ScreenUtils.Companion.getInstance().dp2px(dp);
    }

    /**
     * 同时设置加载更多控件和刷新控件的高度
     *
     * @param refresh  刷新控件的高度 单位为dp
     * @param loadMore 加载控件的高度 单位为dp
     */
    public void setAllHeight(int refresh, int loadMore) {
        head_height = ScreenUtils.Companion.getInstance().dp2px(refresh);
        foot_height = ScreenUtils.Companion.getInstance().dp2px(loadMore);
    }

    /**
     * 设置刷新控件的下拉的最大高度 且必须大于本身控件的高度  最佳为2倍
     *
     * @param dp 单位为dp
     */
    public void setMaxHeadHeight(int dp) {
        if (head_height >= ScreenUtils.Companion.getInstance().dp2px(dp)) {
            return;
        }
        head_height_2 = ScreenUtils.Companion.getInstance().dp2px(dp);
    }

    /**
     * 设置加载更多控件的上拉的最大高度 且必须大于本身控件的高度  最佳为2倍
     *
     * @param dp 单位为dp
     */
    public void setMaxFootHeight(int dp) {
        if (foot_height >= ScreenUtils.Companion.getInstance().dp2px(dp)) {
            return;
        }
        foot_height_2 = ScreenUtils.Companion.getInstance().dp2px(dp);
    }

    /**
     * 同时设置加载更多控件和刷新控件的最大高度 且必须大于本身控件的高度  最佳为2倍
     *
     * @param dp 单位为dp
     */
    public void setAllMaxHeight(int dp) {
        if (head_height >= ScreenUtils.Companion.getInstance().dp2px(dp)) {
            return;
        }
        if (foot_height >= ScreenUtils.Companion.getInstance().dp2px(dp)) {
            return;
        }
        head_height_2 = ScreenUtils.Companion.getInstance().dp2px(dp);
        foot_height_2 = ScreenUtils.Companion.getInstance().dp2px(dp);
    }

    /**
     * 同时设置加载更多控件和刷新控件的最大高度 且必须大于本身控件的高度  最佳为2倍
     *
     * @param refresh  刷新控件下拉的最大高度 单位为dp
     * @param loadMore 加载控件上拉的最大高度 单位为dp
     */
    public void setAllMaxHeight(int refresh, int loadMore) {
        if (head_height >= ScreenUtils.Companion.getInstance().dp2px(refresh)) {
            return;
        }
        if (foot_height >= ScreenUtils.Companion.getInstance().dp2px(loadMore)) {
            return;
        }
        head_height_2 = ScreenUtils.Companion.getInstance().dp2px(refresh);
        foot_height_2 = ScreenUtils.Companion.getInstance().dp2px(loadMore);
    }

    public interface CallBack {
        void onSuccess();
    }
}
