package com.sgm.navi.ui.base;

import android.text.TextUtils;

import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;

import java.util.HashMap;
import java.util.Map;
import java.util.Stack;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

public final class StackManager {
    private Map<String, Stack<BaseActivity>> mBaseActivityStack;
    private Map<String, Stack<BaseFragment>> mBaseFragmentStack;

    private StackManager() {
        mBaseActivityStack = new HashMap<>();
        mBaseFragmentStack = new HashMap<>();
    }

    /**
     * 获取fragment栈的大小.
     *
     * @param screenId 屏幕ID
     * @return 栈大小
     */
    public int getFragmentSize(final String screenId) {
        int size = 0;
        if (ConvertUtils.isEmpty(mBaseFragmentStack)) {
            return size;
        }
        final Stack<BaseFragment> fragments = mBaseFragmentStack.get(screenId);
        if (ConvertUtils.isEmpty(fragments)) {
            return size;
        }
        return fragments.size();
    }

    /***
     *
     * @param screenId
     * @param fragmentSimpleName
     * @return 返回是否存在相同类名的对象
     */
    public boolean isExistFragment(final String screenId,final String fragmentSimpleName) {
        boolean isExist = false;
        if (ConvertUtils.isEmpty(mBaseFragmentStack) || ConvertUtils.isEmpty(mBaseFragmentStack.get(screenId))) {
            isExist = false;
        } else {
           final Stack<BaseFragment> fragments = mBaseFragmentStack.get(screenId);
           for (BaseFragment baseFragment : fragments) {
                if (TextUtils.equals(baseFragment.getClass().getSimpleName(), fragmentSimpleName)) {
                    isExist = true;
                    break;
                }
           }
        }
        return isExist;
    }

    /**
     * 判断栈是否为空.
     *
     * @param screenId 屏幕ID
     * @return true为空
     */
    public boolean isActivityStackNull(final String screenId) {
        final Stack<BaseActivity> baseActivities = ConvertUtils.containToValue(mBaseActivityStack, screenId);
        return ConvertUtils.isEmpty(baseActivities);
    }

    /**
     * 判断栈是否为空.
     *
     * @param screenId 屏幕ID
     * @return true为空
     */
    public boolean isFragmentStackNull(final String screenId) {
        final Stack<BaseFragment> fragmentStack = ConvertUtils.containToValue(mBaseFragmentStack, screenId);
        return ConvertUtils.isEmpty(fragmentStack);
    }

    /**
     * 压栈处理.
     *
     * @param screenId 屏幕ID
     * @return 栈顶元素
     */
    public Stack<BaseActivity> getBaseActivityStack(final String screenId) {
        return ConvertUtils.containToValue(mBaseActivityStack, screenId);
    }

    /**
     * 压栈处理.
     *
     * @param screenId 屏幕ID
     * @return 栈顶元素
     */
    public Stack<BaseFragment> getBaseFragmentStack(final String screenId) {
        return ConvertUtils.containToValue(mBaseFragmentStack, screenId);
    }

    /**
     * 压栈处理.
     *
     * @param screenId 屏幕ID
     * @param baseView 被压栈的视图
     */
    public void push(final String screenId, final IBaseView baseView) {
        if (ConvertUtils.isNullRequire(baseView) instanceof BaseActivity) {
            Stack<BaseActivity> activities = ConvertUtils.containToValue(mBaseActivityStack, screenId);
            activities = ConvertUtils.push(activities, (BaseActivity) baseView);
            mBaseActivityStack.put(screenId, activities);
        } else if (ConvertUtils.isNullRequire(baseView) instanceof BaseFragment) {
            Stack<BaseFragment> fragmentStack = ConvertUtils.containToValue(mBaseFragmentStack, screenId);
            fragmentStack = ConvertUtils.push(fragmentStack, (BaseFragment) baseView);
            mBaseFragmentStack.put(screenId, fragmentStack);
        }
    }

    /**
     * 获取正在显示的视图.
     *
     * @return 视图实例
     */
    @Nullable
    public BaseActivity getFirstActivity() {
        if (ConvertUtils.isEmpty(mBaseActivityStack)) {
            return null;
        }
        final String firstKey = mBaseActivityStack.keySet().stream().findFirst().get();
        if (TextUtils.isEmpty(firstKey)) {
            return null;
        }
        return getCurrentActivity(firstKey);
    }

    /**
     * 获取正在显示的视图.
     *
     * @return 视图实例
     */
    public BaseActivity getMainCurrentActivity() {
        final Stack<BaseActivity> activities = ConvertUtils.containToValue(mBaseActivityStack, "MAIN_SCREEN_MAIN_MAP");
        return ConvertUtils.peek(activities);
    }

    /**
     * 获取正在显示的视图.
     *
     * @param screenId 屏幕UD
     * @return 视图实例
     */
    public BaseActivity getCurrentActivity(final String screenId) {
        final Stack<BaseActivity> activities = ConvertUtils.containToValue(mBaseActivityStack, screenId);
        return ConvertUtils.peek(activities);
    }

    /**
     * 判断Activity是否存在
     *
     * @param screenId 屏幕UD
     * @param cls Activity 类名
     * @return Activity是否存在
     */
    public Boolean isActivityExist(final String screenId,final Class cls) {
        final Stack<BaseActivity> activities = ConvertUtils.containToValue(mBaseActivityStack, screenId);
        AtomicBoolean isExist = new AtomicBoolean(false);
        if (ConvertUtils.isEmpty(activities)) {
            return isExist.get();
        }
        activities.forEach(baseActivity -> {
            if (ConvertUtils.equals(baseActivity.getClass().getSimpleName(),cls.getSimpleName())) {
                isExist.set(true);
            }
        });
        return isExist.get();
    }

    public BaseActivity getActivityByClsName(String screenId, Class cls) {
        AtomicReference<BaseActivity> baseActivity = new AtomicReference<>();
        final Stack<BaseActivity> activities = ConvertUtils.containToValue(mBaseActivityStack, screenId);
        if (ConvertUtils.isEmpty(activities)) {
            return null;
        }
        activities.forEach(activity -> {
            if (ConvertUtils.equals(activity.getClass().getSimpleName(),cls.getSimpleName())) {
                baseActivity.set(activity);
            }
        });
        return baseActivity.get();
    }

    /**
     * 获取正在显示的视图.
     *
     * @param screenId 屏幕UD
     * @return 视图实例
     */
    public BaseFragment getCurrentFragment(final String screenId) {
        final Stack<BaseFragment> fragmentStack = ConvertUtils.containToValue(mBaseFragmentStack, screenId);
        return ConvertUtils.peek(fragmentStack);
    }

    /**
     * 出栈最后一个视图.
     *
     * @param screenId 屏幕UD
     * @return 出栈视图的实例
     */
    public BaseActivity popActivity(final String screenId) {
        if (ConvertUtils.isEmpty(mBaseActivityStack)) {
            return null;
        }
        final Stack<BaseActivity> activityStack = ConvertUtils.containToValue(mBaseActivityStack, screenId);
        return ConvertUtils.pop(activityStack);
    }

    /**
     * 出栈最后一个视图.
     *
     * @param screenId 屏幕UD
     * @return 出栈视图的实例
     */
    public BaseFragment popFragment(final String screenId) {
        if (ConvertUtils.isEmpty(mBaseFragmentStack)) {
            return null;
        }
        final Stack<BaseFragment> fragmentStack = ConvertUtils.containToValue(mBaseFragmentStack, screenId);
        return ConvertUtils.pop(fragmentStack);
    }

    /**
     * 指定视图最后一次出现的位置.
     *
     * @param screenId 屏幕ID
     * @param activity 指定元素
     * @return 元素所在位置
     */
    public int getActivityIndex(final String screenId, final BaseActivity activity) {
        if (ConvertUtils.isEmpty(mBaseActivityStack)) {
            return -1;
        }
        final Stack<BaseActivity> activityStack = ConvertUtils.containToValue(mBaseActivityStack, screenId);
        return ConvertUtils.lastIndexOf(activityStack, activity);
    }

    /**
     * 指定视图最后一次出现的位置.
     *
     * @param screenId 屏幕ID
     * @param fragment 指定元素
     * @return 元素所在位置
     */
    public int getFragmentIndex(final String screenId, final BaseFragment fragment) {
        if (ConvertUtils.isEmpty(mBaseFragmentStack)) {
            return -1;
        }
        final Stack<BaseFragment> fragmentStack = ConvertUtils.containToValue(mBaseFragmentStack, screenId);
        return ConvertUtils.lastIndexOf(fragmentStack, fragment);
    }

    /**
     * 获取指定位置的视图.
     *
     * @param screenId 屏幕ID
     * @param index    指定位置
     * @return 指定位置的元素
     */
    public BaseActivity getIndexActivity(final String screenId, final int index) {
        if (ConvertUtils.isEmpty(mBaseActivityStack)) {
            return null;
        }
        final Stack<BaseActivity> activityStack = ConvertUtils.containToValue(mBaseActivityStack, screenId);
        return ConvertUtils.get(activityStack, index);
    }

    /**
     * 获取指定位置的视图.
     *
     * @param screenId 屏幕ID
     * @param index    指定位置
     * @return 指定位置的元素
     */
    public BaseFragment getIndexFragment(final String screenId, final int index) {
        if (ConvertUtils.isEmpty(mBaseFragmentStack)) {
            return null;
        }
        final Stack<BaseFragment> fragmentStack = ConvertUtils.containToValue(mBaseFragmentStack, screenId);
        return ConvertUtils.get(fragmentStack, index);
    }

    /**
     * 是否包含指定视图
     *
     * @param screenId 屏幕ID
     * @param baseView 指定视图
     * @return true/false
     */
    public boolean isContain(final String screenId, final IBaseView baseView) {
        if (baseView instanceof BaseActivity<?, ?>) {
            final Stack<BaseActivity> activityStack = ConvertUtils.containToValue(mBaseActivityStack, screenId);
            return ConvertUtils.isContain(activityStack, (BaseActivity) baseView);
        } else if (baseView instanceof BaseFragment<?, ?>) {
            final Stack<BaseFragment> fragmentStack = ConvertUtils.containToValue(mBaseFragmentStack, screenId);
            return ConvertUtils.isContain(fragmentStack, (BaseFragment) baseView);
        }
        return false;
    }

    public IBaseView isContainMainActivity(final String screenId) {
        if (isActivityStackNull(screenId)) return null;
        Stack<BaseActivity> activityStack = getBaseActivityStack(screenId);
        for (BaseActivity activity : activityStack) {
            if (ConvertUtils.isEmpty(activity)) continue;
            Logger.d("StackManager", activity.isMapActivity);
            if (activity.isMapActivity) return activity;
        }
        return null;
    }

    public boolean isContainActivity(final String screenId, final String className) {
        if(ConvertUtils.isEmpty(className)) return false;
        if(isActivityStackNull(screenId)) return false;
        Stack<BaseActivity> activityStack = getBaseActivityStack(screenId);
        for (BaseActivity activity : activityStack) {
            if(ConvertUtils.isEmpty(activity)) continue;
            Logger.d("StackManager", activity.getClass().getSimpleName());
            if(ConvertUtils.equals(activity.getClass().getSimpleName(), className)) return true;
        }
        return false;
    }

    /**
     * 是否包含指定视图
     *
     * @param screenId 屏幕ID
     * @param fragment 指定视图
     * @return BaseFragment
     */
    public BaseFragment isContainFragment(final String screenId, final BaseFragment fragment) {
        if (ConvertUtils.isEmpty(mBaseFragmentStack)) {
            return null;
        }
        final Stack<BaseFragment> fragmentStack = ConvertUtils.containToValue(mBaseFragmentStack, screenId);
        if (ConvertUtils.isContain(fragmentStack, fragment)) {
            return getIndexFragment(screenId, getFragmentIndex(screenId, fragment));
        }
        return null;
    }

    /**
     * 移除指定视图
     *
     * @param screenId 屏幕ID
     * @param baseView 指定视图
     */
    public void removeBaseView(final String screenId, final IBaseView baseView) {
        if (baseView instanceof BaseActivity<?, ?>) {
            final Stack<BaseActivity> activityStack = ConvertUtils.containToValue(mBaseActivityStack, screenId);
            ConvertUtils.remove(activityStack, (BaseActivity) baseView);
        } else if (baseView instanceof BaseFragment<?, ?>) {
            final Stack<BaseFragment> fragmentStack = ConvertUtils.containToValue(mBaseFragmentStack, screenId);
            ConvertUtils.remove(fragmentStack, (BaseFragment) baseView);
        }
    }

    /**
     * 移除指定视图
     *
     * @param screenId 屏幕ID
     * @param index    指定位置
     */
    public void removeIndexActivity(final String screenId, final int index) {
        final Stack<BaseActivity> activityStack = ConvertUtils.containToValue(mBaseActivityStack, screenId);
        ConvertUtils.remove(activityStack, index);
    }

    /**
     * 移除指定视图
     *
     * @param screenId 屏幕ID
     * @param index    指定位置
     */
    public void removeIndexFragment(final String screenId, final int index) {
        final Stack<BaseFragment> fragmentStack = ConvertUtils.containToValue(mBaseFragmentStack, screenId);
        ConvertUtils.remove(fragmentStack, index);
    }

    /**
     * 移除所有视图
     *
     * @param screenId 屏幕ID
     */
    public void removeAllActivity(final String screenId) {
        final Stack<BaseActivity> activityStack = ConvertUtils.containToValue(mBaseActivityStack, screenId);
        ConvertUtils.clear(activityStack);
    }

    /**
     * 移除所有视图
     *
     * @param screenId 屏幕ID
     */
    public void removeAllFragment(final String screenId) {
        final Stack<BaseFragment> fragmentStack = ConvertUtils.containToValue(mBaseFragmentStack, screenId);
        ConvertUtils.clear(fragmentStack);
    }

    /**
     * 移除所有视图
     */
    public void removeAllActivity() {
        ConvertUtils.clear(mBaseActivityStack);
        mBaseActivityStack = null;
    }

    /**
     * 移除所有视图
     */
    public void removeAllFragment() {
        ConvertUtils.clear(mBaseFragmentStack);
        mBaseFragmentStack = null;
    }

    /**
     * 销毁栈
     */
    public void destroy() {
        removeAllActivity();
        removeAllFragment();
    }

    /**
     * 退出应用
     */
    public void exitApp() {
        if (Logger.openLog) {
            Logger.printStackTrace("NaviApp_Exit",true);
        }
        System.exit(0);
    }

    public static StackManager getInstance() {
        return Helper.STACK_MANAGER;
    }

    private static final class Helper {
        private static final StackManager STACK_MANAGER = new StackManager();
    }
}
