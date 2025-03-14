package com.fy.navi.ui.base;

import android.text.TextUtils;

import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;

import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/22
 */
public class StackManager {
    private Map<String, Stack<BaseActivity>> baseActivityStack;
    private Map<String, Stack<BaseFragment>> baseFragmentStack;
//    private final Stack<BaseActivity> baseActivityStack;
//    private final Stack<BaseFragment> baseFragmentStack;

    private StackManager() {
//        baseActivityStack = new Stack<>();
//        baseFragmentStack = new Stack<>();
        baseActivityStack = new HashMap<>();
        baseFragmentStack = new HashMap<>();
    }

    public boolean isActivityStackNull(String screenId) {
        Stack<BaseActivity> baseActivities = ConvertUtils.containToValue(baseActivityStack, screenId);
        return ConvertUtils.isEmpty(baseActivities);
    }

    public boolean isFragmentStackNull(String screenId) {
        Stack<BaseFragment> fragmentStack = ConvertUtils.containToValue(baseFragmentStack, screenId);
        return ConvertUtils.isEmpty(fragmentStack);
    }

    public Stack<BaseActivity> getBaseActivityStack(String screenId) {
        return ConvertUtils.containToValue(baseActivityStack, screenId);
    }

    public Stack<BaseFragment> getBaseFragmentStack(String screenId) {
        return ConvertUtils.containToValue(baseFragmentStack, screenId);
    }

    /**
     * 压栈处理.
     * @param screenId 屏幕ID
     * @param baseView 被压栈的视图
     */
    public void push(String screenId, IBaseView baseView) {
        if (ConvertUtils.isNullRequire(baseView) instanceof BaseActivity) {
            Stack<BaseActivity> activities =  ConvertUtils.containToValue(baseActivityStack, screenId);
            activities = ConvertUtils.push(activities, (BaseActivity) baseView);
            baseActivityStack.put(screenId, activities);
        } else if (ConvertUtils.isNullRequire(baseView) instanceof BaseFragment) {
            Stack<BaseFragment> fragmentStack = ConvertUtils.containToValue(baseFragmentStack, screenId);
            fragmentStack = ConvertUtils.push(fragmentStack, (BaseFragment) baseView);
            baseFragmentStack.put(screenId, fragmentStack);
        }
    }

    @Nullable
    public BaseActivity getFirstActivity() {
        if (ConvertUtils.isEmpty(baseActivityStack)) return null;
        String firstKey = baseActivityStack.keySet().stream().findFirst().get();
        if (TextUtils.isEmpty(firstKey)) return null;
        return getCurrentActivity(firstKey);
    }

    /**
     * 获取正在显示的视图.
     * @return 视图实例
     */
    public BaseActivity getMainCurrentActivity() {
        Stack<BaseActivity> activities =  ConvertUtils.containToValue(baseActivityStack, "MAIN_SCREEN_MAIN_MAP");
        return ConvertUtils.peek(activities);
    }

    /**
     * 获取正在显示的视图.
     * @param screenId 屏幕UD
     * @return 视图实例
     */
    public BaseActivity getCurrentActivity(String screenId) {
        Stack<BaseActivity> activities =  ConvertUtils.containToValue(baseActivityStack, screenId);
        return ConvertUtils.peek(activities);
    }

    /**
     * 获取正在显示的视图.
     * @param screenId 屏幕UD
     * @return 视图实例
     */
    public BaseFragment getCurrentFragment(String screenId) {
        Stack<BaseFragment> fragmentStack =  ConvertUtils.containToValue(baseFragmentStack, screenId);
        return ConvertUtils.peek(fragmentStack);
    }

    /**
     * 出栈最后一个视图.
     * @param screenId 屏幕UD
     * @return 出栈视图的实例
     */
    public BaseActivity popActivity(String screenId) {
        if (ConvertUtils.isEmpty(baseActivityStack)) return null;
        Stack<BaseActivity> activityStack = ConvertUtils.containToValue(baseActivityStack, screenId);
        return ConvertUtils.pop(activityStack);
    }

    /**
     * 出栈最后一个视图.
     * @param screenId 屏幕UD
     * @return 出栈视图的实例
     */
    public BaseFragment popFragment(String screenId) {
        if (ConvertUtils.isEmpty(baseFragmentStack)) return null;
        Stack<BaseFragment> fragmentStack = ConvertUtils.containToValue(baseFragmentStack, screenId);
        return ConvertUtils.pop(fragmentStack);
    }

    /**
     * 指定视图最后一次出现的位置.
     * @param screenId 屏幕ID
     * @param activity 指定元素
     * @return 元素所在位置
     */
    public int getActivityIndex(String screenId, BaseActivity activity) {
        if (ConvertUtils.isEmpty(baseActivityStack)) return -1;
        Stack<BaseActivity> activityStack = ConvertUtils.containToValue(baseActivityStack, screenId);
        return ConvertUtils.lastIndexOf(activityStack, activity);
    }

    /**
     * 指定视图最后一次出现的位置.
     * @param screenId 屏幕ID
     * @param fragment 指定元素
     * @return 元素所在位置
     */
    public int getFragmentIndex(String screenId, BaseFragment fragment) {
        if (ConvertUtils.isEmpty(baseFragmentStack)) return -1;
        Stack<BaseFragment> fragmentStack = ConvertUtils.containToValue(baseFragmentStack, screenId);
        return ConvertUtils.lastIndexOf(fragmentStack, fragment);
    }

    /**
     * 获取指定位置的视图.
     * @param screenId 屏幕ID
     * @param index 指定位置
     * @return 指定位置的元素
     */
    public BaseActivity getIndexActivity(String screenId, int index) {
        if (ConvertUtils.isEmpty(baseActivityStack)) return null;
        Stack<BaseActivity> activityStack = ConvertUtils.containToValue(baseActivityStack, screenId);
        return ConvertUtils.get(activityStack, index);
    }

    /**
     * 获取指定位置的视图.
     * @param screenId 屏幕ID
     * @param index 指定位置
     * @return 指定位置的元素
     */
    public BaseFragment getIndexFragment(String screenId, int index) {
        if (ConvertUtils.isEmpty(baseFragmentStack)) return null;
        Stack<BaseFragment> fragmentStack = ConvertUtils.containToValue(baseFragmentStack, screenId);
        return ConvertUtils.get(fragmentStack, index);
    }

    /**
     * 是否包含指定视图
     * @param screenId 屏幕ID
     * @param baseView 指定视图
     * @return true/false
     */
    public boolean isContain(String screenId, IBaseView baseView) {
        if (baseView instanceof BaseActivity<?, ?>) {
            Stack<BaseActivity> activityStack = ConvertUtils.containToValue(baseActivityStack, screenId);
            return ConvertUtils.isContain(activityStack, (BaseActivity) baseView);
        } else if (baseView instanceof BaseFragment<?, ?>) {
            Stack<BaseFragment> fragmentStack = ConvertUtils.containToValue(baseFragmentStack, screenId);
            return ConvertUtils.isContain(fragmentStack, (BaseFragment) baseView);
        }
        return false;
    }

    /**
     * 是否包含指定视图
     * @param screenId 屏幕ID
     * @param fragment 指定视图
     * @return true/false
     */
    public BaseFragment isContainFragment(String screenId, BaseFragment fragment) {
        if (ConvertUtils.isEmpty(baseFragmentStack)) return null;
        Stack<BaseFragment> fragmentStack = ConvertUtils.containToValue(baseFragmentStack, screenId);
        if (ConvertUtils.isContain(fragmentStack, fragment))
            return getIndexFragment(screenId, getFragmentIndex(screenId, fragment));
        return null;
    }

    /**
     * 移除指定视图
     * @param screenId 屏幕ID
     * @param baseView 指定视图
     * @return true/false
     */
    public void removeBaseView(String screenId, IBaseView baseView) {
        if (baseView instanceof BaseActivity<?, ?>) {
            Stack<BaseActivity> activityStack = ConvertUtils.containToValue(baseActivityStack, screenId);
            ConvertUtils.remove(activityStack, (BaseActivity)baseView);
        } else if (baseView instanceof BaseFragment<?, ?>) {
            Stack<BaseFragment> fragmentStack = ConvertUtils.containToValue(baseFragmentStack, screenId);
            ConvertUtils.remove(fragmentStack, (BaseFragment) baseView);
        }
    }

    public void removeIndexActivity(String screenId, int index) {
        Stack<BaseActivity> activityStack = ConvertUtils.containToValue(baseActivityStack, screenId);
        ConvertUtils.remove(activityStack, index);
    }

    public void removeIndexFragment(String screenId, int index) {
        Stack<BaseFragment> fragmentStack = ConvertUtils.containToValue(baseFragmentStack, screenId);
        ConvertUtils.remove(fragmentStack, index);
    }

    public void removeAllActivity(String screenId) {
        Stack<BaseActivity> activityStack = ConvertUtils.containToValue(baseActivityStack, screenId);
        ConvertUtils.clear(activityStack);
    }

    public void removeAllFragment(String screenId) {
        Stack<BaseFragment> fragmentStack = ConvertUtils.containToValue(baseFragmentStack, screenId);
        ConvertUtils.clear(fragmentStack);
    }

    public void removeAllActivity() {
        ConvertUtils.clear(baseActivityStack);
        baseActivityStack = null;
    }

    public void removeAllFragment() {
        ConvertUtils.clear(baseFragmentStack);
        baseFragmentStack = null;
    }

    public void destroy() {
        removeAllActivity();
        removeAllFragment();
    }

    public void exitApp(){
        System.exit(0);
    }

    public static StackManager getInstance() {
        return Helper.stm;
    }

    private static final class Helper {
        private static final StackManager stm = new StackManager();
    }
}
