package com.fy.navi.service.greendao.setting;

import com.android.utils.log.Logger;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.greendao.DaoMaster;

import org.greenrobot.greendao.database.Database;

import java.util.Date;
import java.util.List;


public class SettingManager {
    private static final String TAG = SettingManager.class.getSimpleName();
    private static final String DB_NAME = "setting.db";
    private static SettingManager mDBHelper;
    private NaviSettingDao mSettingDao;

    private boolean mIsInit = false;

    /**
     * Get instance.
     * @return SettingManager
     */
    public static SettingManager getInstance() {
        if (mDBHelper == null) {
            synchronized (SettingManager.class) {
                if (mDBHelper == null) {
                    mDBHelper = new SettingManager();
                }
                return mDBHelper;
            }
        }
        return mDBHelper;
    }

    /**
     * Init database and cloud data.
     */
    public void init() {
        if (mIsInit) {
            Logger.d(TAG, "Database had Initialized!");
            return;
        }
        // 数据库对象
        final DaoMaster.DevOpenHelper helper = new DaoMaster.DevOpenHelper(AppContext.getInstance().getMContext(), DB_NAME);
        final Database db = helper.getWritableDb();
        final DaoMaster daoMaster = new DaoMaster(db);
        mSettingDao = daoMaster.newSession().getNaviSettingDao();
        mIsInit = true;
    }

    /**
     * Use key and value for data settings.
     * @param key save key
     * @param value save value
     */
    public void insertValue(final String key, final String value) {
        Logger.d(TAG, "key = " + key + "  value = " + value);
        if (searchFromDb(key)) {
            Logger.d(TAG, "update");
            update(key, value);
        } else {
            Logger.d(TAG, "insertOrReplace");
            insertOrReplace(key, value);
        }
    }

    /**
     * 查询某个key是否存在于数据库中
     * @param key search key
     * @return true if exist,false if not exist
     */
    private boolean searchFromDb(final String key) {
        final List<NaviSetting> saveData = mSettingDao.queryBuilder()
                .where(NaviSettingDao.Properties.MName.eq(key))
                .list();
        return !saveData.isEmpty();
    }

    /**
     * 修改或者更新设置项数据
     * @param key 对应key值
     * @param value 对应值
     */
    public void insertOrReplace(final String key, final String value) {
        Logger.d(TAG, "insertOrReplace key = " + key + "  value = " + value);
        final NaviSetting setting = new NaviSetting();
        setting.setMName(key);
        setting.setMValue(value);
        setting.setMUpdateTime(new Date());
        mSettingDao.insertOrReplace(setting);
    }

    /**
     * 通过key查找其对应value
     * @param key 唯一值
     * @return value
     */
    public String getValueByKey(final String key) {
        final NaviSetting naviSetting = mSettingDao.queryBuilder()
                .where(NaviSettingDao.Properties.MName.eq(key))
                .unique();
        if (naviSetting != null) {
            return naviSetting.getMValue();
        } else {
            return "";
        }
    }

    /**
     * 查询全部设置数据.
     * @return all data list,String
     */
    private List<NaviSetting> loadAll() {
        final List<NaviSetting> settingList = mSettingDao.loadAll();
        return settingList;
    }

    /**
     * 更新设置项数据
     * @param key 对应key值
     * @param value 对应值
     */
    private void update(final String key, final String value) {
        final NaviSetting unique = mSettingDao.queryBuilder()
                .where(NaviSettingDao.Properties.MName.eq(key))
                .unique();
        unique.setMValue(value);
        mSettingDao.update(unique);
    }

    /**
     * 删除某个设置项数据
     * @param key 对应key值
     */
    public void deleteValue(final String key) {
        mSettingDao.queryBuilder()
                .where(NaviSettingDao.Properties.MName.eq(key))
                .buildDelete()
                .executeDeleteWithoutDetachingEntities();
    }

    /**
     * 清空全部数据
     */
    public void deleteAll() {
        mSettingDao.deleteAll();
    }

}
