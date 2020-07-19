/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

package com.bolour.util;

import java.io.FileInputStream;
import java.io.InputStream;
import java.util.Properties;

public class PropertiesUtil {

    public static Properties readProperties(String path) throws Exception {
        InputStream input = null;
        try {
            input = new FileInputStream(path);
            Properties properties = new Properties();
            properties.load(input);
            return properties;
        }
        finally {
            if (input != null)
                input.close();
        }
    }
}
