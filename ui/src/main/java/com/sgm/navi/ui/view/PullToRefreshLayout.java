package com.sgm.navi.ui.view;

import android.animation.ValueAnimator;
import android.content.Context;
import android.util.AttributeSet;
import android.view.Gravity;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewConfiguration;
import android.view.ViewGroup;

import androidx.core.view.ViewCompat;

import com.android.utils.ScreenUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.ui.view.refresh.FooterView;
import com.sgm.navi.ui.view.refresh.HeadRefreshView;
import com.sgm.navi.ui.view.refresh.HeadView;
import com.sgm.navi.ui.view.refresh.LoadMoreView;
import com.sgm.navi.ui.view.refresh.RefreshListener;
import com.sgm.navi.ui.view.refresh.State;

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

    private boolean mCanLoadMore = true;
    private boolean mCanRefresh = true;
    private boolean mRefresh;
    private boolean mLoadMore;

    //滑动的最小距离
    private int mTouchSlope;

    private RefreshListener mRefreshListener;

    public PullToRefreshLayout(final Context context) {
        this(context, null);
    }

    public PullToRefreshLayout(final Context context, final AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public PullToRefreshLayout(final Context context, final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init();
    }

    /**
     * 设置刷新监听
     *
     * @param refreshListener RefreshListener
     */
    public void setRefreshListener(final RefreshListener refreshListener) {
        this.mRefreshListener = refreshListener;
    }

    /**
     * 计算高度
     */
    private void cal() {
        head_height = ScreenUtils.Companion.getInstance().dp2px(HEAD_HEIGHT);
        foot_height = ScreenUtils.Companion.getInstance().dp2px(FOOT_HEIGHT);
        head_height_2 = ScreenUtils.Companion.getInstance().dp2px(HEAD_HEIGHT * 2);
        foot_height_2 = ScreenUtils.Companion.getInstance().dp2px(FOOT_HEIGHT * 2);
        mTouchSlope = ViewConfiguration.get(getContext()).getScaledTouchSlop();
    }

    /**
     * 初始化
     */
    private void init() {
        cal();
        final int count = getChildCount();
        if(Logger.openLog) {
            Logger.d("SEARCH_HMI_TAG", "count:", count);
        }
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        mChildView = getChildAt(0);
        addHeadView();
        addFooterView();
    }

    /**
     * 添加头布局
     */
    private void addHeadView() {
        if (mHeaderView == null) {
            mHeaderView = new HeadRefreshView(getContext());
        }
        final LayoutParams layoutParams = new LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, 0);
        mHeaderView.getView().setLayoutParams(layoutParams);
        if (mHeaderView.getView().getParent() != null) {
            ((ViewGroup) mHeaderView.getView().getParent()).removeAllViews();
        }
        addView(mHeaderView.getView(), 0);
    }

    /**
     * 添加底部布局
     */
    private void addFooterView() {
        if (mFooterView == null) {
            mFooterView = new LoadMoreView(getContext());
        }
        final LayoutParams layoutParams = new LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, 0);
        layoutParams.gravity = Gravity.BOTTOM;
        mFooterView.getView().setLayoutParams(layoutParams);
        if (mFooterView.getView().getParent() != null) {
            ((ViewGroup) mFooterView.getView().getParent()).removeAllViews();
        }
        addView(mFooterView.getView());
    }

    @Override
    public boolean onInterceptTouchEvent(final MotionEvent ev) {
        if (!mCanLoadMore && !mCanRefresh) {
            return super.onInterceptTouchEvent(ev);
        }
        if (mRefresh || mLoadMore) {
            return true;
        }
        switch (ev.getAction()) {
            case MotionEvent.ACTION_DOWN:
                mTouchY = ev.getY();
                mCurrentY = mTouchY;
                break;
            case MotionEvent.ACTION_MOVE:
                final float currentY = ev.getY();
                final float dy = currentY - mCurrentY;
                if (mCanRefresh) {
                    final boolean canChildScrollUp = canChildScrollUp();
                    if (dy > mTouchSlope && !canChildScrollUp) {
                        mHeaderView.begin();
                        return true;
                    }
                }
                if (mCanLoadMore) {
                    final boolean canChildScrollDown = canChildScrollDown();
                    if (dy < -mTouchSlope && !canChildScrollDown) {
                        mFooterView.begin();
                        return true;
                    }
                }
                break;
            default:
                if(Logger.openLog) {
                    Logger.d("SEARCH_HMI_TAG", "onInterceptTouchEvent");
                }
                break;
        }
        return super.onInterceptTouchEvent(ev);
    }

    @Override
    public boolean onTouchEvent(final MotionEvent event) {
        if (mRefresh || mLoadMore) {
            return true;
        }
        switch (event.getAction()) {
            case MotionEvent.ACTION_MOVE:
                mCurrentY = event.getY();
                float dura = (mCurrentY - mTouchY) / 3.0f;
                if (dura > 0 && mCanRefresh) {
                    mFooterView.getView().getLayoutParams().height = (int) 0;
                    dura = Math.min(head_height_2, dura);
                    dura = Math.max(0, dura);
                    mHeaderView.getView().getLayoutParams().height = (int) dura;
                    ViewCompat.setTranslationY(mChildView, dura);
                    requestLayout();
                    mHeaderView.progress(dura, head_height);
                } else {
                    mHeaderView.getView().getLayoutParams().height = (int) 0;
                    dura = Math.min(foot_height_2, Math.abs(dura));
                    dura = Math.max(0, Math.abs(dura));
                    mFooterView.getView().getLayoutParams().height = (int) dura;
                    ViewCompat.setTranslationY(mChildView, -dura);
                    requestLayout();
                    if (mCanLoadMore) {
                        mFooterView.progress(dura, foot_height);
                    }
                }
                return true;
            case MotionEvent.ACTION_UP:
            case MotionEvent.ACTION_CANCEL:
                final float currentY = event.getY();
                final int dy1 = (int) (currentY - mTouchY) / 3;
                if (dy1 > 0 && mCanRefresh) {
                    if (dy1 >= head_height) {
                        createAnimatorTranslationY(State.RefreshState.REFRESH,
                                dy1 > head_height_2 ? head_height_2 : dy1, head_height,
                                new CallBack() {
                                    @Override
                                    public void onSuccess() {
                                        mRefresh = true;
                                        if (mRefreshListener != null) {
                                            mRefreshListener.refresh();
                                        }
                                        mHeaderView.loading();
                                    }
                                });
                    } else if (dy1 > 0 && dy1 < head_height) {
                        setFinish(dy1, State.RefreshState.REFRESH);
                        mHeaderView.normal();
                    }
                } else {
                    if (Math.abs(dy1) >= foot_height) {
                        createAnimatorTranslationY(State.RefreshState.LOADMORE,
                                Math.abs(dy1) > foot_height_2 ? foot_height_2 : Math.abs(dy1),
                                foot_height,
                                new CallBack() {
                                    @Override
                                    public void onSuccess() {
                                        if (mCanLoadMore) {
                                            mLoadMore = true;
                                            if (mRefreshListener != null) {
                                                mRefreshListener.loadMore();
                                            }
                                            mFooterView.loading();
                                        } else {
                                            setFinish(Math.abs(dy1), State.RefreshState.LOADMORE);
                                            mFooterView.normal();
                                        }
                                    }
                                });
                    } else {
                        setFinish(Math.abs(dy1), State.RefreshState.LOADMORE);
                        mFooterView.normal();
                    }
                }
                break;
            default:
                break;
        }
        return super.onTouchEvent(event);
    }

    /**
     * 是否可以滑动
     *
     * @return true 可以滑动
     */
    private boolean canChildScrollDown() {
        if (mChildView == null) {
            return false;
        }

        return ViewCompat.canScrollVertically(mChildView, 1);
    }

    /**
     * 是否可以滑动
     *
     * @return true 可以滑动
     */
    private boolean canChildScrollUp() {
        if (mChildView == null) {
            return false;
        }
        return ViewCompat.canScrollVertically(mChildView, -1);
    }

    /**
     * 创建动画
     *
     * @param state    RefreshState
     * @param start    开始时间
     * @param purpose  结束时间
     * @param callBack CallBack
     */
    public void createAnimatorTranslationY(@State.RefreshState final int state, final int start, final int purpose, final CallBack callBack) {
        final ValueAnimator anim;
        anim = ValueAnimator.ofInt(start, purpose);
        anim.setDuration(ANIM_TIME);
        anim.addUpdateListener(valueAnimator -> {
            if (valueAnimator.getAnimatedValue() != null) {
                final int value = (int) valueAnimator.getAnimatedValue();
                if (state == State.RefreshState.REFRESH) {
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
     *
     * @param height 刷新高度
     * @param state  刷新状态
     */
    private void setFinish(final int height, @State.RefreshState final int state) {
        createAnimatorTranslationY(state, height, 0, () -> {
            if (state == State.RefreshState.REFRESH) {
                mRefresh = false;
                mHeaderView.normal();
            } else {
                mLoadMore = false;
                mFooterView.normal();
            }
        });
    }

    /**
     * 结束下拉刷新
     *
     * @param state 刷新状态
     */
    private void setFinish(@State.RefreshState final int state) {
        if (state == State.RefreshState.REFRESH) {
            if (mHeaderView != null && mHeaderView.getView().getLayoutParams().height > 0 && mRefresh) {
                setFinish(head_height, state);
            }
        } else {
            if (mFooterView != null && mFooterView.getView().getLayoutParams().height > 0 && mLoadMore) {
                setFinish(foot_height, state);
            }
        }
    }

    /**
     * 设置下拉刷新提示语
     *
     * @param tips 提示语
     */
    public void setRefreshTips(final String tips) {
        if (mHeaderView != null) {
            mHeaderView.setRefreshTips(tips);
        }
    }

    /**
     * 设置加载更多提示语
     *
     * @param tips 提示语
     */
    public void setLoadMoreTips(final String tips) {
        if (mFooterView != null) {
            mFooterView.setLoadMoreTips(tips);
        }
    }

    /**
     * 结束刷新
     */
    public void finishRefresh() {
        setFinish(State.RefreshState.REFRESH);
    }

    /**
     * 结束加载更多
     */
    public void finishLoadMore() {
        setFinish(State.RefreshState.LOADMORE);
    }

    /**
     * 设置是否启用加载更多
     *
     * @param canLoadMore true 启用 false 不启用
     */
    public void setCanLoadMore(final boolean canLoadMore) {
        this.mCanLoadMore = canLoadMore;
    }

    /**
     * 设置是否启用刷新
     *
     * @param canRefresh true 启用 false 不启用
     */
    public void setCanRefresh(final boolean canRefresh) {
        this.mCanRefresh = canRefresh;
    }

    /**
     * 设置是否正在刷新
     *
     * @param isRefresh true 正在刷新 false 不在刷新
     */
    public void setRefresh(final boolean isRefresh) {
        if (mHeaderView != null) {
            mHeaderView.setRefresh(isRefresh);
        }
    }

    /**
     * 设置是下拉刷新头部
     *
     * @param headerView 需实现 HeadView 接口
     */
    public void setHeaderView(final HeadView headerView) {
        this.mHeaderView = headerView;
    }

    /**
     * 设置是下拉刷新尾部
     *
     * @param footerView 需实现 FooterView 接口
     */
    public void setFooterView(final FooterView footerView) {
        this.mFooterView = footerView;
    }

    /**
     * 设置刷新控件的高度
     *
     * @param dp 单位为dp
     */
    public void setHeadHeight(final int dp) {
        head_height = ScreenUtils.Companion.getInstance().dp2px(dp);
    }

    /**
     * 设置加载更多控件的高度
     *
     * @param dp 单位为dp
     */
    public void setFootHeight(final int dp) {
        foot_height = ScreenUtils.Companion.getInstance().dp2px(dp);
    }

    /**
     * 同时设置加载更多控件和刷新控件的高度
     *
     * @param dp 单位为dp
     */
    public void setAllHeight(final int dp) {
        head_height = ScreenUtils.Companion.getInstance().dp2px(dp);
        foot_height = ScreenUtils.Companion.getInstance().dp2px(dp);
    }

    /**
     * 同时设置加载更多控件和刷新控件的高度
     *
     * @param refresh  刷新控件的高度 单位为dp
     * @param loadMore 加载控件的高度 单位为dp
     */
    public void setAllHeight(final int refresh, final int loadMore) {
        head_height = ScreenUtils.Companion.getInstance().dp2px(refresh);
        foot_height = ScreenUtils.Companion.getInstance().dp2px(loadMore);
    }

    /**
     * 设置刷新控件的下拉的最大高度 且必须大于本身控件的高度  最佳为2倍
     *
     * @param dp 单位为dp
     */
    public void setMaxHeadHeight(final int dp) {
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
    public void setMaxFootHeight(final int dp) {
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
    public void setAllMaxHeight(final int dp) {
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
    public void setAllMaxHeight(final int refresh, final int loadMore) {
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
        /**
         * 动画执行完毕
         */
        void onSuccess();
    }
}
