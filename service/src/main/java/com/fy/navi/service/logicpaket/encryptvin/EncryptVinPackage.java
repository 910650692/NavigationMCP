package com.fy.navi.service.logicpaket.encryptvin;

import com.sgm.vinsdk.PLATFORMS;
import com.sgm.vinsdk.VinHelper;

public final class EncryptVinPackage {

    private EncryptVinPackage() {
    }

    public static EncryptVinPackage getInstance() {
        return Helper.ENCRYPT_VIN_PACKAGE;
    }

    private static final class Helper {
        private static final EncryptVinPackage ENCRYPT_VIN_PACKAGE = new EncryptVinPackage();
    }


    //VinHelper.getVin();
}
